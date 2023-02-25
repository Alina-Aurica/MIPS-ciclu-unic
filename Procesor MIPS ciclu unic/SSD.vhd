----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/25/2022 06:41:33 PM
-- Design Name: 
-- Module Name: SSD - Behavioral
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

entity SSD is
    Port ( dig : in STD_LOGIC_VECTOR (15 downto 0);
           clk : in STD_LOGIC;
           an : out STD_LOGIC_VECTOR (3 downto 0);
           cat : out STD_LOGIC_VECTOR (6 downto 0));
end SSD;

architecture Behavioral of SSD is

signal cnt: STD_LOGIC_VECTOR (15 downto 0);
signal int_cat: STD_LOGIC_VECTOR (3 downto 0);

begin

process(clk)
begin
     if rising_edge(clk) then
       cnt <= cnt + 1;
    end if;
end process;

process(cnt(15), cnt(14))
begin
    if cnt(15) = '0' and cnt(14) = '0' then 
        an <= "0111";
    elsif cnt(15) = '0' and cnt(14) = '1' then 
        an <= "1011";
    elsif cnt(15) = '1' and cnt(14) = '0' then
        an <= "1101";
    else an <= "1110";
    end if;
end process;

process(cnt(15), cnt(14), dig)
begin
    if cnt(15) = '0' and cnt(14) = '0' then 
        int_cat <= dig(15 downto 12);
    elsif cnt(15) = '0' and cnt(14) = '1' then 
        int_cat <= dig(11 downto 8);
    elsif cnt(15) = '1' and cnt(14) = '0' then
        int_cat <= dig(7 downto 4);
    else int_cat <= dig(3 downto 0);
    end if;
end process;

with int_cat select
   cat<= "1111001" when "0001",   --1
         "0100100" when "0010",   --2
         "0110000" when "0011",   --3
         "0011001" when "0100",   --4
         "0010010" when "0101",   --5
         "0000010" when "0110",   --6
         "1111000" when "0111",   --7
         "0000000" when "1000",   --8
         "0010000" when "1001",   --9
         "0001000" when "1010",   --A
         "0000011" when "1011",   --b
         "1000110" when "1100",   --C
         "0100001" when "1101",   --d
         "0000110" when "1110",   --E
         "0001110" when "1111",   --F
         "1000000" when others;   --0

end Behavioral;
