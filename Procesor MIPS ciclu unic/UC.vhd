----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 03/26/2022 07:44:17 AM
-- Design Name: 
-- Module Name: UC - Behavioral
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

entity UC is
    Port (instr: in STD_LOGIC_VECTOR (15 downto 0);
          RegDst: out STD_LOGIC;
          ExtOp: out STD_LOGIC;
          ALUSrc: out STD_LOGIC;
          Branch: out STD_LOGIC;
          Jump: out STD_LOGIC;
          ALUOp: out STD_LOGIC_VECTOR (2 downto 0);
          MemWrite: out STD_LOGIC;
          MemtoReg: out STD_LOGIC;
          RegWrite: out STD_LOGIC);
end UC;

architecture Behavioral of UC is

begin

    process(instr(15 downto 13))
    begin
        RegDst <= '0';
        ExtOp <= '0';
        ALUSrc <= '0';
        Branch <= '0';
        Jump <= '0';
        ALUOp <= "000";
        MemWrite <= '0';
        MemtoReg <= '0';
        RegWrite <= '0';
        
        case instr(15 downto 13) is
            when "000" => RegDst <= '1'; ALUOp <= "000"; RegWrite <= '1';
            when "001" => ALUSrc <= '1'; ALUOp <= "001"; RegWrite <= '1'; ExtOp <= '1';
            when "010" => ALUSrc <= '1'; MemtoReg <= '1'; ALUOp <= "010"; ExtOp <= '1'; RegWrite <= '1';
            when "011" => ALUSrc <= '1'; MemWrite <= '1'; ALUOp <= "011"; RegWrite <= '1'; ExtOp <= '1';
            when "100" => ALUSrc <= '1'; Branch <= '1'; ALUOp <= "100"; ExtOp <= '1';
            when "101" => ALUSrc <= '1'; ALUOp <= "101"; ExtOp <= '1'; RegWrite <= '1';
            when "110" => ALUSrc <= '1'; ALUOp <= "110"; ExtOp <= '1'; RegWrite <= '1';
            when others => Jump <= '1';
        end case;
    end process;


end Behavioral;
