LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY data_memory0 IS
    PORT(
        CLK, R  : IN STD_LOGIC;
        INDEX   : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
        DATA_IN : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        WR      : IN STD_LOGIC;
        CS      : IN STD_LOGIC;
        DATA_OUT: OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END data_memory0;

ARCHITECTURE Behavioral OF data_memory0 IS

    TYPE custom_memory IS ARRAY(0 TO 63) OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    CONSTANT default_memory : custom_memory := (
        0 => x"11", 1 => x"22", 2 => x"33", 3 => x"44",
        4 => x"55", 5 => x"66", 6 => x"77", 7 => x"88",
        8 => x"99", 9 => x"AA", 10 => x"BB", 11 => x"CC",
        12 => x"DD", 13 => x"EE", 14 => x"FF", 15 => x"00",
        16 => x"01", 17 => x"02", 18 => x"03", 19 => x"04",
        20 => x"05", 21 => x"06", 22 => x"07", 23 => x"08",
        24 => x"09", 25 => x"0A", 26 => x"0B", 27 => x"0C",
        28 => x"0D", 29 => x"0E", 30 => x"0F", 31 => x"10",
        32 => x"21", 33 => x"32", 34 => x"43", 35 => x"54",
        36 => x"65", 37 => x"76", 38 => x"87", 39 => x"98",
        40 => x"A9", 41 => x"BA", 42 => x"CB", 43 => x"DC",
        44 => x"ED", 45 => x"FE", 46 => x"EF", 47 => x"F0",
        48 => x"12", 49 => x"34", 50 => x"56", 51 => x"78",
        52 => x"9A", 53 => x"BC", 54 => x"DE", 55 => x"F1",
        56 => x"13", 57 => x"35", 58 => x"57", 59 => x"79",
        60 => x"9B", 61 => x"BD", 62 => x"DF", 63 => x"F2"
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
