LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY tb_cu IS
END tb_cu;

ARCHITECTURE Behavioral OF tb_cu IS
    COMPONENT cu
        PORT(
            clk, reset: IN STD_LOGIC;
            hit : IN STD_LOGIC;
            write, update: OUT STD_LOGIC
        );
    END COMPONENT;

    SIGNAL clk, reset, hit : STD_LOGIC := '0';
    SIGNAL write, update : STD_LOGIC;

    CONSTANT clk_period : time := 10 ns;
BEGIN
    uut: cu PORT MAP(
        clk => clk,
        reset => reset,
        hit => hit,
        write => write,
        update => update
    );

    clk_process: PROCESS
    BEGIN
        clk <= '0';
        WAIT FOR clk_period / 2;
        clk <= '1';
        WAIT FOR clk_period / 2;
    END PROCESS;

    stim_proc: PROCESS
    BEGIN
        reset <= '1';
        WAIT FOR clk_period;
        reset <= '0';

        -- Test SEARCH state (hit = 0)
        hit <= '0';
        WAIT FOR clk_period * 2;

        -- Test SEARCH to RDWR transition (hit = 1)
        hit <= '1';
        WAIT FOR clk_period * 2;

        -- Test UPDATED state (hit = 0)
        hit <= '0';
        WAIT FOR clk_period * 2;

        WAIT;
    END PROCESS;
END Behavioral;
