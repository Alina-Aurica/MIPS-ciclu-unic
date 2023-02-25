----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 03/25/2022 04:39:01 PM
-- Design Name: 
-- Module Name: ID_comp - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity ID_comp is
    Port ( clk: in STD_LOGIC;
           en: in STD_LOGIC;
           instr : in STD_LOGIC_VECTOR (15 downto 0);
           RegW : in STD_LOGIC;
           rd1 : out STD_LOGIC_VECTOR (15 downto 0);
           rd2 : out STD_LOGIC_VECTOR (15 downto 0);
           RegDst : in STD_LOGIC;
           wd : in STD_LOGIC_VECTOR (15 downto 0);
           ExtOp : in STD_LOGIC;
           ExtImm : out STD_LOGIC_VECTOR (15 downto 0);
           func : out STD_LOGIC_VECTOR (2 downto 0);
           sa: out STD_LOGIC);
end ID_comp;

architecture Behavioral of ID_comp is

signal wa: STD_LOGIC_VECTOR (2 downto 0);

type reg_array is array (0 to 7) of STD_LOGIC_VECTOR (15 downto 0);
signal reg_file: reg_array := (0 => x"0000", 
                               1 => "0000000000000011", 
                               --1 => "0000000000000000", 
                               2 => "0000000000000010", 
                               --2 => "0000000000000000", 
                               3 => "0000000000000100", 
                               5 => "0000000000000101", 
                               6 => "0000000000000100",
                               --6 => "0000000000000000",
                               7 => "0000000000000111",  
                               others=>x"0000");


begin

process(RegDst)
begin
    if RegDst = '1' then
        wa <= instr(6 downto 4);
    else
        wa <= instr(9 downto 7);
    end if;
end process;

process(clk)
begin
    if rising_edge(clk) then
        if RegW = '1' then
            reg_file(conv_integer(wa)) <= wd;
--        else
--             reg_file(conv_integer(wa)) <= wd;
        end if;
    end if;
end process;

rd1 <= reg_file(conv_integer(instr(12 downto 10)));
rd2 <= reg_file(conv_integer(instr(9 downto 7)));

func <= instr(2 downto 0);
sa <= instr(3);

process(ExtOp)
begin
    if ExtOp = '1' then
        if instr(6) = '1' then
            ExtImm <= "111111111" & instr(6 downto 0);
        else
            ExtImm <= "000000000" & instr(6 downto 0);
        end if;
    else
        ExtImm <= "000000000" & instr(6 downto 0);
     end if;
end process;

end Behavioral;
