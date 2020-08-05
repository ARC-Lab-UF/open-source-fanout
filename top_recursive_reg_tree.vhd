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

-- TOP_RECURSIVE_REG_TREE
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top_recursive_reg_tree is
    generic(
        data_width      : natural := 2;  -- width of register
        num_output      : natural := 2;  -- n number of fanout to input signal
        max_fanout      : natural := 2;   -- maximum fanout length before duplication occurs
        reserve_regions : natural := 2;
		combo			: boolean := false);  -- n number of reserved regions
    port(
        clk     : in std_logic;
        input   : in  std_logic_vector(data_width-1 downto 0);
        output  : out std_logic_vector(data_width*num_output-1 downto 0);
        res_out : out std_logic_vector(reserve_regions-1 downto 0));
end top_recursive_reg_tree;

architecture GEN of top_recursive_reg_tree is

    signal input_r 	    : std_logic_vector(input'range);    -- output from input register
    signal output_r	    : std_logic_vector(output'range);   -- output from test entitygin

begin

    -- Generate the reserved regions
    U_RESERVE_GEN: for i in 0 to reserve_regions-1 generate
        U_RESERVE : entity work.reg
            generic map (
                width   => 1)
            port map (
                clk     => clk,
                input  	=> (others => '0'),
                output 	=> res_out(((i+1))-1) downto (i));
    end generate;

    -- Registered input to get accurate timing information
    U_INPUTS : entity work.reg
        generic map (
            width   => data_width)
        port map (
            clk     => clk,
            input  	=> input,
            output 	=> input_r);

	U_TREE : entity work.recursive_reg_tree
		generic map (
			num_output   		=> num_output,
			data_width  		=> data_width,
			max_fanout  		=> max_fanout,
			register_input  	=> true)
	  port map (
			clk    => clk,
			input  => input_r,
			output => output_r);

    -- Generate the output registers
    U_OUTPUT_GEN: for i in 0 to num_output-1 generate
        U_OUTPUT : entity work.reg
            generic map (
                width   => data_width)
            port map (
                clk     => clk,
                input   => output_r(((i+1)*data_width-1) downto (data_width*i)),
                output 	=> output(((i+1)*data_width-1) downto (data_width*i)));
    end generate;
end GEN;

