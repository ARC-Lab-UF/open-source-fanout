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

-- REG
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity reg is
    generic(
		width  	: positive);
    port(
		clk    	: in  std_logic;
        input  	: in  std_logic_vector(width-1 downto 0);
        output 	: out std_logic_vector(width-1 downto 0));
end reg;

architecture FF of reg is

	 signal in_out : std_logic_vector(width-1 downto 0);
	 attribute preserve: boolean; 
	 attribute preserve of in_out: signal is true;

    
begin  -- BHV
	process(clk)
	begin
		if (rising_edge(clk)) then
			in_out <= input;
		end if;
	end process;
	
	output <= in_out;
	
end FF;
