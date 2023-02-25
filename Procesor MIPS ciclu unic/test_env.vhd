----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/18/2022 06:27:31 PM
-- Design Name: 
-- Module Name: test_env - Behavioral
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

entity test_env is
    Port ( clk : in STD_LOGIC;
           btn : in STD_LOGIC_VECTOR (4 downto 0);
           sw : in STD_LOGIC_VECTOR (15 downto 0);
           led : out STD_LOGIC_VECTOR (15 downto 0);
           an : out STD_LOGIC_VECTOR (3 downto 0);
           cat : out STD_LOGIC_VECTOR (6 downto 0));
end test_env;

architecture Behavioral of test_env is

component MPG
    Port( btn: IN STD_LOGIC;
          clk: IN STD_LOGIC;
          en: OUT STD_LOGIC);
end component;

component SSD
     Port( dig : in STD_LOGIC_VECTOR (15 downto 0);
           clk : in STD_LOGIC;
           an : out STD_LOGIC_VECTOR (3 downto 0);
           cat : out STD_LOGIC_VECTOR (6 downto 0));
end component;

--component RF
--    Port ( clk : in STD_LOGIC;
--           ra1 : in STD_LOGIC_VECTOR (3 downto 0);
--           ra2 : in STD_LOGIC_VECTOR (3 downto 0);
--           wa : in STD_LOGIC_VECTOR (3 downto 0);
--           wd : in STD_LOGIC_VECTOR (15 downto 0);
--           wen : in STD_LOGIC;
--           rd1 : out STD_LOGIC_VECTOR (15 downto 0);
--           rd2 : out STD_LOGIC_VECTOR (15 downto 0));
--end component;

--component RAM
--    Port ( clk : in STD_LOGIC;
--           we : in STD_LOGIC;
--           en : in STD_LOGIC;
--           addr : in STD_LOGIC_VECTOR (7 downto 0);
--           di : in STD_LOGIC_VECTOR (15 downto 0);
--           do : out STD_LOGIC_VECTOR (15 downto 0));
--end component;

component IF_comp 
    Port ( en : in STD_LOGIC;
           clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           JAdr : in STD_LOGIC_VECTOR (15 downto 0);
           BAdr : in STD_LOGIC_VECTOR (15 downto 0);
           PC1 : inout STD_LOGIC_VECTOR (15 downto 0);
           Instr : out STD_LOGIC_VECTOR (15 downto 0);
           PCSrc: in STD_LOGIC;
           Jump: in STD_LOGIC);
end component;

component ID_comp
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
end component;

component UC is
     Port (instr: in STD_LOGIC_VECTOR (15 downto 0);
         RegDst: out STD_LOGIC := '0';
         ExtOp: out STD_LOGIC := '0';
         ALUSrc: out STD_LOGIC := '0';
         Branch: out STD_LOGIC := '0';
         Jump: out STD_LOGIC := '0';
         ALUOp: out STD_LOGIC_VECTOR (2 downto 0) := "000";
         MemWrite: out STD_LOGIC := '0';
         MemtoReg: out STD_LOGIC := '0';
         RegWrite: out STD_LOGIC := '0');
end component;

component ALU is
    Port ( rd1 : in STD_LOGIC_VECTOR (15 downto 0);
           rd2 : in STD_LOGIC_VECTOR (15 downto 0);
           Ext_Imm : in STD_LOGIC_VECTOR (15 downto 0);
           ALUSrc : in STD_LOGIC;
           sa : in STD_LOGIC;
           func : in STD_LOGIC_VECTOR (2 downto 0);
           ALUOp : in STD_LOGIC_VECTOR (2 downto 0);
           ALURez : inout STD_LOGIC_VECTOR (15 downto 0);
           Zero : out STD_LOGIC);
end component;

component MEM is
    Port ( clk : in STD_LOGIC;
           MemWrite : in STD_LOGIC;
           ALURes : inout STD_LOGIC_VECTOR (15 downto 0);
           rd2 : in STD_LOGIC_VECTOR (15 downto 0);
           MemData : out STD_LOGIC_VECTOR (15 downto 0));
end component;

signal cnt: STD_LOGIC_VECTOR (15 downto 0);
signal en: STD_LOGIC;
signal rst: STD_LOGIC;
signal en1: STD_LOGIC;
--type Rom_type is array(0 to 255) of STD_LOGIC_VECTOR (15 downto 0);
--signal ROM: Rom_type := (b"000_001_010_101_0_000", b"000_101_100_011_0_001", b"000_011_111_110_0_010", b"000_101_100_010_0_011", b"000_111_001_011_0_100", b"000_111_110_010_0_101", b"000_011_101_001_0_110", b"000_010_110_100_0_111", b"001_011_010_0000100", b"010_100_011_0000010", b"011_101_100_0000111", b"100_100_001_0000011", b"101_010_100_0000110", b"110_011_010_0000100", others => b"111_0000000001100");
signal R_D: STD_LOGIC_VECTOR(15 downto 0);
signal wd: STD_LOGIC_VECTOR(15 downto 0);
signal rd1: STD_LOGIC_VECTOR(15 downto 0);
signal rd2: STD_LOGIC_VECTOR(15 downto 0);
signal ALURez: STD_LOGIC_VECTOR(15 downto 0);
signal di: STD_LOGIC_VECTOR(15 downto 0);
signal do: STD_LOGIC_VECTOR(15 downto 0);
signal PC1: STD_LOGIC_VECTOR(15 downto 0);
signal Instr: STD_LOGIC_VECTOR(15 downto 0);
signal SMux: STD_LOGIC_VECTOR(15 downto 0);
signal RegDst: STD_LOGIC;
signal ExtOp: STD_LOGIC;
signal ExtImm: STD_LOGIC_VECTOR(15 downto 0);
signal func: STD_LOGIC_VECTOR(2 downto 0);
signal sa: STD_LOGIC;
signal RegW: STD_LOGIC;
signal digits: STD_LOGIC_VECTOR (15 downto 0);

signal MemData: STD_LOGIC_VECTOR (15 downto 0);

signal ALUSrc: STD_LOGIC;
signal Branch: STD_LOGIC;
signal Jump: STD_LOGIC;
signal ALUOp: STD_LOGIC_VECTOR (2 downto 0);
signal MemWrite: STD_LOGIC;
signal MemtoReg: STD_LOGIC;
signal RegWrite: STD_LOGIC;
signal Zero: STD_LOGIC;


signal Ext_func: STD_LOGIC_VECTOR (15 downto 0);
signal Ext_sa: STD_LOGIC_VECTOR (15 downto 0);

signal PCSrc: STD_LOGIC;

signal JAddr: STD_LOGIC_VECTOR (15 downto 0);
signal BAddr: STD_LOGIC_VECTOR (15 downto 0);



--signal A: STD_LOGIC_VECTOR (15 downto 0);
--signal A1: STD_LOGIC_VECTOR (15 downto 0);
--signal B: STD_LOGIC_VECTOR (15 downto 0);
--signal Add: STD_LOGIC_VECTOR (15 downto 0);
--signal Dif: STD_LOGIC_VECTOR (15 downto 0);
--signal Shright: STD_LOGIC_VECTOR (15 downto 0);
--signal Shleft: STD_LOGIC_VECTOR (15 downto 0);
--signal dig: STD_LOGIC_VECTOR (15 downto 0);

begin
-- led <= sw;
-- an <= btn(3 downto 0);
-- cat <= (others => '0');

leg1: MPG port map (btn(0), clk, en);
leg2: SSD port map (digits, clk, an, cat);
leg3: MPG port map (btn(1), clk, rst);
--leg4: RF port map (clk, cnt(3 downto 0), cnt(3 downto 0), cnt(3 downto 0), wd, en1, rd1, rd2);
--leg5: RAM port map (clk, en1, '1', cnt(7 downto 0), di, do);

leg6: IF_comp port map (en, clk, rst, JAddr , BAddr, PC1, Instr, PCSrc, Jump);  
leg7: ID_comp port map (clk, en, Instr, RegWrite, rd1, rd2, RegDst, wd, ExtOp, ExtImm, func, sa); 
leg8: UC port map (Instr, RegDst, ExtOp, ALUSrc, Branch, Jump, ALUOp, MemWrite, MemtoReg, RegWrite);
leg9: ALU port map (rd1, rd2, ExtImm, ALUSrc, sa, func, ALUOp, ALURez, Zero);
leg10: MEM port map (clk, MemWrite, ALURez, rd2, MemData);

PCSrc <= Branch AND Zero;

process(MemtoReg)
begin
    --if MemWrite = '1' then
        if MemtoReg = '1' then
            wd <= MemData;
        else
            wd <= ALURez;
        end if;
    --end if;
end process;

JAddr <= PC1(15 downto 13) & Instr(12 downto 0);
BAddr <= PC1 + ExtImm;

process(sw(7), PC1, Instr)
begin
    if sw(7) = '1' then
        SMux <= PC1;
    else
        SMux <= Instr;
    end if;
end process;

--wd <= rd1 + rd2;

Ext_func <= "0000000000000" & func;
Ext_sa <= "000000000000000" & sa;

process(sw(7 downto 5))
begin
    case SW(7 downto 5) is
        when "000" => digits <= PC1 - 1;
        when "001" => digits <= Instr;
        when "010" => digits <= rd1;
        when "011" => digits <= rd2;
        when "100" => digits <= ExtImm;
        when "101" => digits <= ALURez;
        when "110" => digits <= MemData;
        when others => digits <= wd;
    end case;
end process;

--with sw(7 downto 5) select
    --digits <= Instr when "000", PC1 when "001", rd1 when "010", rd2 when "011", wd when "100", ExtImm when "101", Ext_func when "110", Ext_sa when "111", (others => 'X') when others;
 
led(10 downto 0) <= ALUOp & RegDst & ExtOp & ALUSrc & Branch & Jump & MemWrite & MemtoReg & RegWrite;


process(clk)
begin
    if rising_edge(clk) then
        if en = '1' then
            if sw(0) = '1' then
                cnt <= cnt + 1;
                else 
                cnt <= cnt - 1;
            end if;
        end if;
     end if;
end process;

--A <= x"000" & Sw(3 downto 0);
--B <= x"000" & Sw(7 downto 4);
--Add <= A + B;
--Dif <= A - B;
--A1 <= "00000000" & Sw(7 downto 0);
--Shright <= "00" & A1(15 downto 2);
--Shleft <= A1(13 downto 0) & "00";

--process(cnt(15), cnt(14))
--begin
--    if cnt(15 downto 14) = "00" then 
--        dig <= Add;
--    elsif cnt(15 downto 14) = "01" then
--        dig <= Dif;
--    elsif cnt(15 downto 14) = "10" then
--        dig <= Shright;
--    elsif cnt(15 downto 14) = "11" then 
--        dig <= Shleft;
--    end if;
--end process;

--R_D <= ROM(conv_integer(cnt));

--wd <= rd1+rd2;

--di<=do(13 downto 0)& "00";

--process(sw(7), PC1, Instr)
--begin
--    if sw(7) = '1' then
--        SMux <= PC1;
--    else
--        SMux <= Instr;
--    end if;
--end process;
 
end Behavioral;
