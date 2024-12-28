library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity cmp is
    port(
        input1, input2 : in std_logic_vector(7 downto 0);
        result : out std_logic
    );
end cmp;

architecture Behavioral of cmp is

begin
    result <= '1' when input1 = input2 else '0';
end Behavioral;
