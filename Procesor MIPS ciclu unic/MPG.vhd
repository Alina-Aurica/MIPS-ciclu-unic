library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity MPG is
    Port( btn: IN STD_LOGIC;
          clk: IN STD_LOGIC;
          en: OUT STD_LOGIC);
end MPG;

architecture Behavioral of MPG is
signal cnt: STD_LOGIC_VECTOR (15 downto 0);
signal encnt: STD_LOGIC;
signal Q1: STD_LOGIC;
signal Q2: STD_LOGIC;
signal Q3: STD_LOGIC;

begin

en <= Q2 AND (not Q3);

process(clk)
begin
     if rising_edge(clk) then
       cnt <= cnt + 1;
    end if;
end process;

encnt <= '1' when cnt = x"FFFF"
        else '0';

process(clk)
begin
    if rising_edge(clk) then
        if encnt = '1' then
            Q1 <= btn;
        end if;
    end if;
end process;

process(clk)
begin
    if rising_edge(clk) then
        Q2 <= Q1;
        Q3 <= Q2;
    end if;
end process;

end Behavioral;