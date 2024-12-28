LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY data_memory1 IS
    PORT(
        CLK, R  : IN STD_LOGIC;
        INDEX   : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
        DATA_IN : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        WR      : IN STD_LOGIC;
        CS      : IN STD_LOGIC;
        DATA_OUT: OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END data_memory1;

ARCHITECTURE Behavioral OF data_memory1 IS

    TYPE custom_memory IS ARRAY(0 TO 63) OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    CONSTANT default_memory : custom_memory := (
        0 => x"12", 1 => x"23", 2 => x"34", 3 => x"45",
        4 => x"56", 5 => x"67", 6 => x"78", 7 => x"89",
        8 => x"9A", 9 => x"AB", 10 => x"BC", 11 => x"CD",
        12 => x"DE", 13 => x"EF", 14 => x"F0", 15 => x"01",
        16 => x"13", 17 => x"24", 18 => x"35", 19 => x"46",
        20 => x"57", 21 => x"68", 22 => x"79", 23 => x"8A",
        24 => x"9B", 25 => x"AC", 26 => x"BD", 27 => x"CE",
        28 => x"DF", 29 => x"F1", 30 => x"02", 31 => x"14",
        32 => x"25", 33 => x"36", 34 => x"47", 35 => x"58",
        36 => x"69", 37 => x"7A", 38 => x"8B", 39 => x"9C",
        40 => x"AD", 41 => x"BE", 42 => x"CF", 43 => x"E0",
        44 => x"F2", 45 => x"03", 46 => x"15", 47 => x"26",
        48 => x"37", 49 => x"48", 50 => x"59", 51 => x"6A",
        52 => x"7B", 53 => x"8C", 54 => x"9D", 55 => x"AE",
        56 => x"BF", 57 => x"D0", 58 => x"E1", 59 => x"F3",
        60 => x"04", 61 => x"16", 62 => x"27", 63 => x"38"
    );

    SIGNAL internal_data : custom_memory := default_memory;

BEGIN
    PROCESS(CLK, R)
    BEGIN
        IF R = '1' THEN
            internal_data <= default_memory;
        ELSIF rising_edge(CLK) THEN
            IF WR = '1' AND CS = '1' THEN
                internal_data(TO_INTEGER(UNSIGNED(index))) <= DATA_IN;
            END IF;
        END IF;
    END PROCESS;

    DATA_OUT <= internal_data(TO_INTEGER(UNSIGNED(index)));
END Behavioral;
