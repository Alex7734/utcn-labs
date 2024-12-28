LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY tag_memory IS
    PORT(
        CLK, R  : IN STD_LOGIC;
        INDEX   : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
        DATA_IN : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        WR      : IN STD_LOGIC;
        CS      : IN STD_LOGIC;
        DATA_OUT: OUT STD_LOGIC_VECTOR(8 DOWNTO 0)
    );
END tag_memory;

ARCHITECTURE Behavioral OF tag_memory IS
    TYPE custom_memory IS ARRAY(0 TO 63) OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    CONSTANT default_memory : custom_memory := (
        0 => x"AB", 1 => x"4F", 2 => x"92", 3 => x"3D",
        4 => x"7E", 5 => x"1A", 6 => x"BC", 7 => x"58",
        8 => x"6C", 9 => x"2E", 10 => x"F3", 11 => x"8B",
        12 => x"D4", 13 => x"9F", 14 => x"5A", 15 => x"7B",
        16 => x"3C", 17 => x"8E", 18 => x"1D", 19 => x"6A",
        20 => x"4B", 21 => x"9C", 22 => x"2F", 23 => x"7D",
        24 => x"5E", 25 => x"1B", 26 => x"AC", 27 => x"4D",
        28 => x"6F", 29 => x"3E", 30 => x"8A", 31 => x"5B",
        32 => x"7C", 33 => x"2D", 34 => x"9E", 35 => x"1F",
        36 => x"4A", 37 => x"6B", 38 => x"3C", 39 => x"8D",
        40 => x"5E", 41 => x"7F", 42 => x"2A", 43 => x"9B",
        44 => x"1C", 45 => x"4D", 46 => x"6E", 47 => x"3F",
        48 => x"8B", 49 => x"5C", 50 => x"7D", 51 => x"2E",
        52 => x"9F", 53 => x"1A", 54 => x"4B", 55 => x"6C",
        56 => x"3D", 57 => x"8E", 58 => x"5F", 59 => x"7A",
        60 => x"2B", 61 => x"9C", 62 => x"1D", 63 => x"4E"
    );

    SIGNAL internal_data : custom_memory := default_memory;

    SIGNAL valids : std_logic_vector(0 TO 63) := (OTHERS => '1');

BEGIN
    PROCESS(CLK, R)
    BEGIN
        IF R = '1' THEN
            internal_data <= default_memory;
            valids <= (OTHERS => '1');
        ELSIF rising_edge(CLK) THEN
            IF WR = '1' AND CS = '1' THEN
                internal_data(TO_INTEGER(UNSIGNED(index))) <= DATA_IN;
            END IF;
        END IF;
    END PROCESS;

    DATA_OUT(8) <= valids(TO_INTEGER(UNSIGNED(index)));
    DATA_OUT(7 DOWNTO 0) <= internal_data(TO_INTEGER(UNSIGNED(index)));
END Behavioral;
