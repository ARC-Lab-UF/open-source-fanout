--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Authored By: Greg Stitt
-- Reauthored By: Austin Baylis
-- Modified By: Madison N Emas
-- University of Florida
-- ARC LAB UF
-- https://github.com/ARC-Lab-UF/open-source-fanout

-- MIT License

-- Copyright (c) [2020] [University of Florida]

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- RECURSIVE_REG_TREE
-- Recursive implementation of a (a,b)-tree:
--  https://en.wikipedia.org/wiki/(a,b)-tree
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity recursive_reg_tree is
  generic (
    -- User Settings
    num_output     : positive;         -- Number of outputs being fanned-out to
    data_width      : positive;         -- Width of input data
    max_fanout      : positive;         -- Maximum allowable fan-out per node
    register_input  : boolean := false; -- Add root node register
    register_output : boolean := false; -- Register individual outputs
    balanced        : boolean := true;  -- Ensure all outputs arriave at the same time
    -- Internal Use
    is_top          : boolean  := true  -- Identifies top of recursion
  );
  port (
    clk    : in  std_logic;
    rst    : in  std_logic := '0';
    en     : in  std_logic := '1';
    input  : in  std_logic_vector(data_width-1 downto 0);
    output : out std_logic_vector(num_output*data_width-1 downto 0)
  );
end recursive_reg_tree;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
architecture recursive of recursive_reg_tree is

  -- Custom array types
  type reg_array_t  is array (natural range <>) of std_logic_vector(data_width-1 downto 0);
  type int_array_t  is array (natural range <>) of integer;
  type bool_array_t is array (natural range <>) of boolean;

  -- Signals
  signal input_s : std_logic_vector(input'range);

begin

  ------------------------------------------------------------------------------
  -- U_REG_INPUT
  -- If registered input is requested, and it is the top, register the input.
  ------------------------------------------------------------------------------
  U_REG_INPUT : if is_top and register_input generate
    process(clk, rst)
    begin
      if (rst = '1') then
        input_s <= (others => '0');
      elsif (rising_edge(clk)) then
        if (en = '1') then
          input_s <= input;
        end if;
      end if;
    end process;
  end generate U_REG_INPUT;

  ------------------------------------------------------------------------------
  -- U_NO_REG_INPUT
  -- If registered input is not requested, or it is not the top, just pass the
  --  input signal through.
  ------------------------------------------------------------------------------
  U_NO_REG_INPUT : if not(is_top and register_input) generate
    input_s <= input;
  end generate U_NO_REG_INPUT;

  ------------------------------------------------------------------------------
  -- U_BASE_REG
  -- The registered base case is defined as follows:
  --  If the number of outputs is less-than or equal-to the maximum allowable
  --   fan-out, register the input then connect it to the output.
  ------------------------------------------------------------------------------
  U_BASE_REG : if num_output <= max_fanout and register_output generate

    signal output_array : reg_array_t(0 to num_output-1);

    -- Ensure the identical registers don't get merged.
    attribute dont_merge                 : boolean;
    attribute dont_merge of output_array : signal is true;

  -- Register the input
  begin
    process(clk, rst)
    begin
      if (rst = '1') then
        for i in 0 to num_output-1 loop
          output_array(i) <= (others => '0');
        end loop;
      elsif (rising_edge(clk)) then
        if (en = '1') then
          for i in 0 to num_output-1 loop
            output_array(i) <= input_s;
          end loop;
        end if;
      end if;
    end process;

    -- Vectorize the output array
    process(output_array)
    begin
      for i in 0 to num_output-1 loop
        output((i+1)*data_width-1 downto i*data_width) <= output_array(i);
      end loop;
    end process;
  end generate U_BASE_REG;

  ------------------------------------------------------------------------------
  -- U_BASE_NO_REG
  -- The non-registered base case is defined as follows:
  --  If the number of outputs is less-than or equal-to the maximum allowable
  --   fan-out, fan-out the input directly to the outputs.
  ------------------------------------------------------------------------------
  U_BASE_NO_REG : if num_output <= max_fanout and not register_output generate
  begin
    -- Fan-out the input to the outputs
    process(input_s)
    begin
      for i in 0 to num_output-1 loop
        output((i+1)*data_width-1 downto i*data_width) <= input_s;
      end loop;
    end process;
  end generate U_BASE_NO_REG;

  ------------------------------------------------------------------------------
  -- U_RECURSE
  -- If the inputs to the entity do not meet the base conditoins, then use the
  --  recursive definiton to generate the tree.
  ------------------------------------------------------------------------------
  U_RECURSE : if num_output > max_fanout generate

    ----------------------------------------------------------------------------
    -- get_num_output
    -- Using the minimum number of outputs per child, the number of additional
    --  nodes that will be required, and the number of extra outputs, return
    --  a list that gives the final number of outputs per child.
    ----------------------------------------------------------------------------
    function get_num_output(min_out_per_child, extra_nodes, extra_outputs : integer) return int_array_t is

      variable out_per_child : int_array_t(0 to max_fanout-1);

    begin
      -- Set up the minimum outputs per child
      for i in 0 to max_fanout-1 loop
        out_per_child(i) := min_out_per_child;
      end loop;

      -- Assign the extra nodes the extra outputs
      if extra_nodes > 0 then
        for i in 0 to extra_outputs-1 loop
          out_per_child(i mod extra_nodes) := out_per_child(i mod extra_nodes) + 1;
        end loop;
      end if;

      return out_per_child;
    end function;

    ----------------------------------------------------------------------------
    -- get_num_children
    -- Very simple function that takes the array of outputs per child and checks
    --  for non-zero enntries. The returned value is the number of non-zero
    --  entries.
    ----------------------------------------------------------------------------
    function get_num_children(out_per_child : int_array_t) return integer is
      variable num_children : integer;
    begin
      num_children := 0;

      for i in 0 to max_fanout-1 loop
        if out_per_child(i) /= 0 then
          num_children := num_children + 1;
        end if;
      end loop;

      return num_children;
    end function;

    ----------------------------------------------------------------------------
    -- get_out_ranges
    -- Since the number of outputs per child varies, it's hard to make a loop
    --  that can calculate the ranges appropraitley. This function returns the
    --  end points of the data ranges. This is then used later to create the
    --  ranges.
    ----------------------------------------------------------------------------
    function get_out_ranges(out_per_child : int_array_t; num_children : integer) return int_array_t is

      variable child_out_ranges : int_array_t(0 to num_children);

    begin
      -- The first element is -1 because the loop is easiest to write when the
      --  lower element is incremented by 1. In that case, -1 becomes 0 for the
      --  first iteration of the loop.
      child_out_ranges(0) := -1;

      -- From here on out, the calculation for when data ranges should end is
      --  fairly straight forward. The range will end where the previous one
      --  ended plus the total number of bits in the current range (outputs per
      --  child * data width)
      ----------------------------------------------------------------------------
      for i in 1 to num_children loop
        child_out_ranges(i) := child_out_ranges(i-1) + (out_per_child(i-1) * data_width);
      end loop;

      return child_out_ranges;
    end function;

    ----------------------------------------------------------------------------
    -- clog_pos()
    -- Description: Computes ceil(logbase(input)), but with clogbase(1)=1 instead of 0
    -- Useful for getting minimum bits needed to index "input" number of
    -- distinct values, such as for addresses, with base 2.
    ----------------------------------------------------------------------------
    function clog_pos(input : positive; base : positive) return positive is
    begin
        if (input = 1) then
            return 1;
        end if;

        return positive(ceil(log(real(input), real(base))));
    end function;

    ----------------------------------------------------------------------------
    -- balance_nodes
    -- This function is used to balance the children so the outputs arrive at
    --  the same time. First, we determine how many cycles each child will need
    --  to complete the fan-out. At the same time, we determine the depth of the
    --  shortest path.
    -- After that, we enable the flag to trigger the balancing register for each
    --  child that has same cycles left as the shortest path.
    ----------------------------------------------------------------------------
    function balance_nodes(out_per_child : int_array_t; num_children : integer) return bool_array_t is

      variable cycles_left         : int_array_t(0 to num_children-1);
      variable shortest_path       : integer;
      variable need_more_cycles    : boolean;
      variable every_path_shortest : boolean;

      variable node_needs_balance : bool_array_t(0 to num_children-1);

    begin
      -- By default, we shouldn't need any extra cycles
      need_more_cycles := false;

      -- Find number of cycles each path will need.
      for i in 0 to num_children-1 loop
        cycles_left(i) := clog_pos(out_per_child(i), max_fanout);

        -- Identify the shortest path
        if i = 0 then
          shortest_path := cycles_left(i);
        else
          if cycles_left(i) < shortest_path then
            shortest_path := cycles_left(i);
          end if;
        end if;

        -- If a path needs more time, set flag
        if cycles_left(i) > 1 then
          need_more_cycles := true;
        end if;
      end loop;

      -- If every path is the shortest path, then we don't need the delay
      every_path_shortest := true;

      for i in 0 to num_children-1 loop
        if cycles_left(i) /= shortest_path then
          every_path_shortest := false;
        end if;
      end loop;

      -- If path has 1 cycle left, but another path needs more time, then delay
      --  the path
      for i in 0 to num_children-1 loop
        if cycles_left(i) = shortest_path and need_more_cycles = true and every_path_shortest = false then
          node_needs_balance(i) := true;
        else
          node_needs_balance(i) := false;
        end if;
      end loop;


      return node_needs_balance;
    end function;


    ----------------------------------------------------------------------------
    -- Constants
    ----------------------------------------------------------------------------
    -- Tree Construction
    constant NUM_REQ_NODES        : integer := integer(ceil(real(num_output)/real(max_fanout)));
    constant MIN_NODES_PER_CHILD  : integer := integer(floor(real(NUM_REQ_NODES)/real(max_fanout)));
    constant MIN_OUT_PER_CHILD    : integer := MIN_NODES_PER_CHILD * max_fanout;
    constant EXTRA_NODES          : integer := NUM_REQ_NODES mod max_fanout;
    constant EXTRA_OUTPUTS        : integer := num_output - (MIN_OUT_PER_CHILD * max_fanout);

    -- Child Information
    constant OUTPUTS_PER_CHILD    : int_array_t := get_num_output(MIN_OUT_PER_CHILD, EXTRA_NODES, EXTRA_OUTPUTS);
    constant NUM_CHILDREN         : integer     := get_num_children(OUTPUTS_PER_CHILD);
    constant CHILD_OUT_RANGES     : int_array_t := get_out_ranges(OUTPUTS_PER_CHILD, NUM_CHILDREN);

    -- Balance Information
    constant NODE_NEEDS_BALANCE   : bool_array_t := balance_nodes(OUTPUTS_PER_CHILD, NUM_CHILDREN);

    ----------------------------------------------------------------------------
    -- Signals
    ----------------------------------------------------------------------------
    signal reg_array : reg_array_t(0 to NUM_CHILDREN-1);
    signal reg_array2 : reg_array_t(0 to NUM_CHILDREN-1);

    -- Ensure the identical registers don't get merged.
    attribute dont_merge              : boolean;
    attribute dont_merge of reg_array : signal is true;
    attribute dont_merge of reg_array2 : signal is true;

  begin
    process(clk, rst)
    begin
      if (rst = '1') then
        for i in 0 to NUM_CHILDREN-1 loop
          reg_array(i) <= (others => '0');
        end loop;
      elsif (rising_edge(clk)) then
        if (en = '1') then
          for i in 0 to NUM_CHILDREN-1 loop
            reg_array(i) <= input_s;
          end loop;
        end if;
      end if;
    end process;

    ----------------------------------------------------------------------------
    -- U_BALANCE_NODES
    -- Sometimes, depending on the configuration of the tree, the child nodes
    --  may need additional registers to balance the tree. We register the input
    --  to minimize the number of registers used.
    ----------------------------------------------------------------------------
    U_BALANCE_NODES : for i in 0 to NUM_CHILDREN-1 generate
      --------------------------------------------------------------------------
      -- U_BALANCE_NODE
      -- If the tree is configured to be balanced, and the node requires it,
      --  add a register to the child's input.
      --------------------------------------------------------------------------
      U_BALANCE_NODE : if NODE_NEEDS_BALANCE(i) = true  and balanced = true generate
        -- TODO: We should replace this with a register entitiy to make the tree
        --  more elastic.
        process(clk, rst)
        begin
          if (rst = '1') then
            reg_array2(i) <= (others => '0');
          elsif (rising_edge(clk)) then
            if (en = '1') then
              reg_array2(i) <= reg_array(i);
            end if;
          end if;
        end process;
      end generate U_BALANCE_NODE;

      --------------------------------------------------------------------------
      -- U_NO_BALANCE_NODE
      -- If the tree is not configured to be balanced, or the node does not need
      --  to be balanced, then do not insert the register.
      --------------------------------------------------------------------------
      U_NO_BALANCE_NODE : if not (NODE_NEEDS_BALANCE(i) = true  and balanced = true) generate
        reg_array2(i) <= reg_array(i);
      end generate U_NO_BALANCE_NODE;
    end generate U_BALANCE_NODES;

    ----------------------------------------------------------------------------
    -- U_CHILDREN
    -- If the child exists, create a child entity for the tree. The trick is to
    --  connect the outputs correctly. We have to access the previous child's
    --  output range, so we start at 1 to avoid issues.
    ----------------------------------------------------------------------------
    U_CHILDREN : for i in 1 to NUM_CHILDREN generate
      U_CHILD_EXISTS : if OUTPUTS_PER_CHILD(i-1) /= 0 generate
        U_CHILD : entity work.recursive_reg_tree
          generic map (
            num_output     => OUTPUTS_PER_CHILD(i-1),
            data_width      => data_width,
            max_fanout      => max_fanout,
            register_output => register_output,
            is_top          => false
            )
          port map (
            clk    => clk,
            rst    => rst,
            en     => en,
            input  => reg_array2(i-1),
            output => output(CHILD_OUT_RANGES(i) downto CHILD_OUT_RANGES(i-1) + 1)
            );
      end generate U_CHILD_EXISTS;
    end generate U_CHILDREN;
  end generate U_RECURSE;
end recursive;
