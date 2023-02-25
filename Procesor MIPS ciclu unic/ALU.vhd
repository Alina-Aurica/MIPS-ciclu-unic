----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04/01/2022 07:14:04 PM
-- Design Name: 
-- Module Name: ALU - Behavioral
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

entity ALU is
    Port ( rd1 : in STD_LOGIC_VECTOR (15 downto 0);
           rd2 : in STD_LOGIC_VECTOR (15 downto 0);
           Ext_Imm : in STD_LOGIC_VECTOR (15 downto 0);
           ALUSrc : in STD_LOGIC;
           sa : in STD_LOGIC;
           func : in STD_LOGIC_VECTOR (2 downto 0);
           ALUOp : in STD_LOGIC_VECTOR (2 downto 0);
           ALURez : inout STD_LOGIC_VECTOR (15 downto 0);
           Zero : out STD_LOGIC);
           
end ALU;

architecture Behavioral of ALU is
signal rd2Mux: STD_LOGIC_VECTOR (15 downto 0);
signal Add: STD_LOGIC_VECTOR (15 downto 0);
signal Dif: STD_LOGIC_VECTOR (15 downto 0);
signal Shright: STD_LOGIC_VECTOR (15 downto 0);
signal Shleft: STD_LOGIC_VECTOR (15 downto 0);
signal si: STD_LOGIC_VECTOR (15 downto 0);
signal sau: STD_LOGIC_VECTOR (15 downto 0);
signal sau_exclusiv: STD_LOGIC_VECTOR (15 downto 0);
signal notsau: STD_LOGIC_VECTOR (15 downto 0);
signal ALUCtrl: STD_LOGIC_VECTOR (2 downto 0);

begin

Add <= rd1 + rd2Mux;
Dif <= rd1 - rd2Mux;
Shright <= "00" & rd1(15 downto 2);
Shleft <= rd1(13 downto 0) & "00";
si <= rd1 AND rd2Mux;
sau <= rd1 OR rd2Mux;
sau_exclusiv <= rd1 XOR rd2Mux;
notsau <= rd1 nor rd2Mux;

process(ALUSrc)
begin 
    if ALUSrc = '1' then
        rd2Mux <= Ext_Imm;
    else
        rd2Mux <= rd2;
    end if;
end process;


process(func, sa, ALUOp)
begin
    if ALUOp = "000" then
        case func is
            when "000" => ALUCtrl <= "000";
            when "001" => ALUCtrl <= "001";
            when "010" => ALUCtrl <= "010";
            when "011" => ALUCtrl <= "011";
            when "100" => ALUCtrl <= "100";
            when "101" => ALUCtrl <= "101";
            when "110" => ALUCtrl <= "110";
            when others => ALUCtrl <= "111";
       end case;
    else
        if ALUOp = "001" then 
            ALUCtrl <= "000";
        else
            if ALUOp = "101" then
                ALUCtrl <= "101";
            else
                if ALUOp = "110" then
                    ALUCtrl <= "110";
                else
                    if ALUOp = "100" then
                        ALUCtrl <= "100";
                    else
                        ALUCtrl <= "000";
                    end if;
                end if;
            end if;
        end if;
    end if;  
        
end process;

process(ALUCtrl, sa)
begin
     if sa = '0' then
        case ALUCtrl is
            when "000" => ALURez <= Add;
            when "001" => ALURez <= Dif;
            when "100" => ALURez <= si;
            when "101" => ALURez <= sau;
            when "110" => ALURez <= sau_exclusiv;
            when "111" => ALURez <= notsau;
            when others => ALURez <= x"0000";
        end case;
    end if;
  
    if sa = '1' then
        case ALUCtrl is
            when "010" => ALURez <= Shleft;
            when "011" => ALURez <= Shright;
            when others => ALURez <= x"0000";
        end case;
    end if;
       
end process;

process(ALUOp, ALURez)
begin
    if ALUCtrl = "100" then
        if ALURez = x"0000" then
            Zero <= '1';
        else
            Zero <= '0';
        end if;
    else
        Zero <= '0';
    end if;
end process;

end Behavioral;
