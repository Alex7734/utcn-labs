LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY tb IS
END tb;

ARCHITECTURE Behavioral OF tb IS

    COMPONENT top IS
        PORT(
            address : IN std_logic_vector(15 downto 0);
            clk, reset : IN std_logic;
            dout : OUT std_logic_vector(7 downto 0);
            mem0_out, mem1_out, mem2_out, mem3_out, tag_out : OUT std_logic_vector(7 downto 0);
            hit_signal : OUT std_logic
        );
    END COMPONENT;

    CONSTANT t : time := 10 ns;

    SIGNAL clk, reset : std_logic := '0';
    SIGNAL addr : std_logic_vector(15 downto 0);
    SIGNAL dout : std_logic_vector(7 downto 0);
    SIGNAL mem0_out, mem1_out, mem2_out, mem3_out, tag_out : std_logic_vector(7 downto 0);
    SIGNAL hit : std_logic;

BEGIN

    uut: top PORT MAP(
        address => addr,
        clk => clk,
        reset => reset,
        dout => dout,
        mem0_out => mem0_out,
        mem1_out => mem1_out,
        mem2_out => mem2_out,
        mem3_out => mem3_out,
        tag_out => tag_out,
        hit_signal => hit
    );

    reset <= '1', '0' AFTER 3 ns, '1' AFTER 80ns, '0' AFTER 85ns;  

    clk <= NOT clk AFTER t / 2;

    addr <= x"1515" AFTER 5 ns,    -- Tag = x"15", Index = 5,  RAM = data_memory1
            x"2424" AFTER 25 ns,   -- Tag = x"24", Index = 9,  RAM = data_memory0
            x"3737" AFTER 50 ns,   -- Tag = x"37", Index = 13, RAM = data_memory3
            x"1d48" AFTER 85 ns,   -- Tag = x"1e", Index = 18, RAM = data_memory0
            x"2759" AFTER 130 ns;  -- Tag = x"2f", Index = 22, RAM = data_memory1

END Behavioral;
