----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 03/24/2022 11:02:34 AM
-- Design Name: 
-- Module Name: IF - Behavioral
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

entity IF_comp is
    Port ( en : in STD_LOGIC;
           clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           JAdr : in STD_LOGIC_VECTOR (15 downto 0);
           BAdr : in STD_LOGIC_VECTOR (15 downto 0);
           PC1 : inout STD_LOGIC_VECTOR (15 downto 0);
           Instr : out STD_LOGIC_VECTOR (15 downto 0);
           PCSrc: in STD_LOGIC;
           Jump: in STD_LOGIC);
end IF_comp;

architecture Behavioral of IF_comp is

signal D: STD_LOGIC_VECTOR(15 downto 0);
signal Adr: STD_LOGIC_VECTOR(15 downto 0);
type Rom_type is array(0 to 255) of STD_LOGIC_VECTOR (15 downto 0);
--signal ROM: Rom_type := (b"000_001_010_101_0_000", b"000_101_100_011_0_001", b"000_011_111_110_1_010", b"000_101_100_010_1_011", b"000_111_001_011_0_100", b"000_111_110_010_0_101", b"000_011_101_001_0_110", b"000_010_110_100_0_111", b"001_011_010_0000100", b"010_100_011_0000010", b"011_101_100_0000111", b"100_100_001_0000011", b"101_010_100_0000110", b"110_011_010_0000100", others => b"111_0000000001100");
signal ROM: Rom_type := (b"000_001_010_101_0_000", --add 0550 rd1=3 rd2=2 ALURez=5 (wd)
                         b"000_101_000_011_1_010", --sll 143A rd1=5 rd2=0 ALURez=14(hexazecimal, 20(zecimal) (wd)
                         b"000_011_110_100_0_001", --sub 1f41 rd1=14 rd2=4 ALURez=10(hezazecimal), 16(zecimal) (wd)
                         b"000_100_000_111_1_011", --srl 107b rd1=10 rd2=0 ALURez=4 (wd)
                         b"001_111_001_0000111", --addi 3c87 rd1=4 ExtImm=7 ALURez=b(hexazecimal), 11(zecimal) (wd)
                         --b"001_111_001_0000000", --addi 3c87 rd1=4 ExtImm=7 ALURez=b(hexazecimal), 11(zecimal) (wd)
                         b"011_001_101_0000000", --sw 6680 rd1=b rd2=b ALURez=b, in memorie, tot b
                         b"010_101_010_0000000", --lw 5500 rd1=b rd2=b, in wd ecrie b
                         b"100_001_000_0000011", --bqe 8403 rd1=b rd2=0 ExtImm=3
                         b"000_001_100_110_0_100", --and 0664 rd1=b rd2=10 ALURez=0 (wd)
                         b"000_110_100_011_0_101", --or 1a35 rd1=0 rd2=10 ALURez=10 (wd)
                         b"111_0000000001101", --j e00d 
                         b"101_001_011_0000111", --ori a587 rd1=0 rd2=7 ExtImm=7 ALURez=7 (wd)
                         b"110_011_100_0000010", --xori ce02 rd1=7 rd2=5 ExtImm=2 ALURez=5 (wd)
                         b"000_011_010_100_0_110", --xor 0d46 rd1=10 rd2=2 ALURez=1b(hexazecimal), 27(zecimal) (wd)
                         others => b"000_100_001_111_0_111"); --nor 10f7 rd1=1b rd2=4 ALURez=FFE4(hexazecimal) (wd)
signal JMux: STD_LOGIC_VECTOR(15 downto 0);

begin

process(clk)
begin
    if rising_edge(clk) then
        if en = '1' then
            if rst = '0' then
                Adr <= D;
            else 
                Adr <= x"0000";
            end if;
        end if;
    end if;
end process;

Instr <= ROM(conv_integer(Adr));

PC1 <= Adr + 1;

process(PCSrc, PC1, BAdr)
begin
    if PCSrc = '1' then
        JMux <= BAdr;
    else
        JMux <= PC1;
    end if;
end process;

process(Jump, JMux, JAdr)
begin
    if Jump = '1' then
        D <= JAdr;
    else
        D <= JMux;
    end if;
end process;


end Behavioral;
