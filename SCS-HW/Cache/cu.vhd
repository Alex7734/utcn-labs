LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY cu IS
    PORT(
        clk, reset: IN STD_LOGIC;
        hit : IN STD_LOGIC;
        write, update: OUT STD_LOGIC
    );
END cu;

ARCHITECTURE Behavioral OF cu IS
    TYPE state_type IS (SEARCH, UPDATED, RDWR);
    SIGNAL current_state : state_type := SEARCH;
BEGIN
    fsm: PROCESS(clk, reset)
    BEGIN
        IF reset = '1' THEN
            current_state <= SEARCH;
            write <= '0';
            update <= '0';
        ELSIF rising_edge(clk) THEN
            CASE current_state IS
                WHEN SEARCH =>
                    update <= '0';
                    write <= '0';
                    IF hit = '1' THEN
                        current_state <= RDWR;
                    ELSE
                        current_state <= UPDATED;
                    END IF;
                WHEN UPDATED =>
                    update <= '1';
                    write <= '1';
                    current_state <= RDWR;
                WHEN RDWR =>
                    update <= '0';
                    write <= '1';
                    current_state <= SEARCH;
            END CASE;
        END IF;
    END PROCESS;
END Behavioral;
