library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity decoder is
    Port(
        select_signal : in std_logic_vector(1 downto 0);
        output_vector : out std_logic_vector(0 to 3) 
    );
end decoder;

architecture Behavioral of decoder is

begin
    with select_signal select 
        output_vector <= 
            "1000" when "00",
            "0100" when "01",
            "0010" when "10",
            "0001" when others;

end Behavioral;
