LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY data_memory3 IS
    PORT(
        CLK, R  : IN STD_LOGIC;
        INDEX   : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
        DATA_IN : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        WR      : IN STD_LOGIC;
        CS      : IN STD_LOGIC;
        DATA_OUT: OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END data_memory3;

ARCHITECTURE Behavioral OF data_memory3 IS

    TYPE custom_memory IS ARRAY(0 TO 63) OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    CONSTANT default_memory : custom_memory := (
        0 => x"15", 1 => x"26", 2 => x"37", 3 => x"48",
        4 => x"59", 5 => x"6A", 6 => x"7B", 7 => x"8C",
        8 => x"9D", 9 => x"AE", 10 => x"BF", 11 => x"D0",
        12 => x"E1", 13 => x"F2", 14 => x"03", 15 => x"14",
        16 => x"25", 17 => x"36", 18 => x"47", 19 => x"58",
        20 => x"69", 21 => x"7A", 22 => x"8B", 23 => x"9C",
        24 => x"AD", 25 => x"BE", 26 => x"CF", 27 => x"E0",
        28 => x"F1", 29 => x"02", 30 => x"13", 31 => x"24",
        32 => x"35", 33 => x"46", 34 => x"57", 35 => x"68",
        36 => x"79", 37 => x"8A", 38 => x"9B", 39 => x"AC",
        40 => x"BD", 41 => x"CE", 42 => x"DF", 43 => x"F0",
        44 => x"01", 45 => x"12", 46 => x"23", 47 => x"34",
        48 => x"45", 49 => x"56", 50 => x"67", 51 => x"78",
        52 => x"89", 53 => x"9A", 54 => x"AB", 55 => x"BC",
        56 => x"CD", 57 => x"DE", 58 => x"EF", 59 => x"00",
        60 => x"10", 61 => x"20", 62 => x"30", 63 => x"40"
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
