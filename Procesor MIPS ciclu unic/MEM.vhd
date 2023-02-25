----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04/07/2022 05:44:55 PM
-- Design Name: 
-- Module Name: MEM - Behavioral
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

entity MEM is
    Port ( clk : in STD_LOGIC;
           MemWrite: in STD_LOGIC; 
           ALURes : inout STD_LOGIC_VECTOR (15 downto 0);
           rd2 : in STD_LOGIC_VECTOR (15 downto 0);
           MemData : out STD_LOGIC_VECTOR (15 downto 0));
end MEM;

architecture Behavioral of MEM is

type ram_type is array(0 to 255) of STD_LOGIC_VECTOR(15 downto 0);
signal RAM: ram_type;

begin

process(clk)
begin
    if clk'event and clk='1' then
        if MemWrite='1' then
            RAM(conv_integer(ALURes(4 downto 0))) <= rd2;
        end if;
    end if;
end process;

MemData <= RAM(conv_integer(ALURes(4 downto 0)));

end Behavioral;
