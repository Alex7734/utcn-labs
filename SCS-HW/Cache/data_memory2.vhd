LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY data_memory2 IS
    PORT(
        CLK, R  : IN STD_LOGIC;
        INDEX   : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
        DATA_IN : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        WR      : IN STD_LOGIC;
        CS      : IN STD_LOGIC;
        DATA_OUT: OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END data_memory2;

ARCHITECTURE Behavioral OF data_memory2 IS

    TYPE custom_memory IS ARRAY(0 TO 63) OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    CONSTANT default_memory : custom_memory := (
        0 => x"13", 1 => x"24", 2 => x"35", 3 => x"46",
        4 => x"57", 5 => x"68", 6 => x"79", 7 => x"8A",
        8 => x"9B", 9 => x"AC", 10 => x"BD", 11 => x"CE",
        12 => x"DF", 13 => x"F0", 14 => x"01", 15 => x"12",
        16 => x"23", 17 => x"34", 18 => x"45", 19 => x"56",
        20 => x"67", 21 => x"78", 22 => x"89", 23 => x"9A",
        24 => x"AB", 25 => x"BC", 26 => x"CD", 27 => x"DE",
        28 => x"EF", 29 => x"F1", 30 => x"02", 31 => x"13",
        32 => x"24", 33 => x"35", 34 => x"46", 35 => x"57",
        36 => x"68", 37 => x"79", 38 => x"8A", 39 => x"9B",
        40 => x"AC", 41 => x"BD", 42 => x"CE", 43 => x"DF",
        44 => x"F0", 45 => x"01", 46 => x"12", 47 => x"23",
        48 => x"34", 49 => x"45", 50 => x"56", 51 => x"67",
        52 => x"78", 53 => x"89", 54 => x"9A", 55 => x"AB",
        56 => x"BC", 57 => x"CD", 58 => x"DE", 59 => x"EF",
        60 => x"F1", 61 => x"02", 62 => x"13", 63 => x"24"
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
