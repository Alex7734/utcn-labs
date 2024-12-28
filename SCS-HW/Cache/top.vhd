LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY top IS
    PORT(
        address : IN std_logic_vector(15 downto 0);
        clk, reset : IN std_logic;
        dout : OUT std_logic_vector(7 downto 0);
        mem0_out, mem1_out, mem2_out, mem3_out, tag_out : OUT std_logic_vector(7 downto 0);
        hit_signal : OUT std_logic
    );
END top;

ARCHITECTURE Structural OF top IS

    COMPONENT decoder IS
        PORT(
            select_signal : IN std_logic_vector(1 downto 0);
            output_vector : OUT std_logic_vector(0 TO 3)
        );
    END COMPONENT;

    COMPONENT tag_memory IS
        PORT(
            clk, R : IN std_logic;
            index : IN std_logic_vector(5 downto 0);
            data_in : IN std_logic_vector(7 downto 0);
            wr : IN std_logic;
            cs : IN std_logic;
            data_out : OUT std_logic_vector(8 downto 0)
        );
    END COMPONENT;

    COMPONENT data_memory0 IS
        PORT(
            clk, R : IN std_logic;
            index : IN std_logic_vector(5 downto 0);
            data_in : IN std_logic_vector(7 downto 0);
            wr : IN std_logic;
            cs : IN std_logic;
            data_out : OUT std_logic_vector(7 downto 0)
        );
    END COMPONENT;

    COMPONENT data_memory1 IS
        PORT(
            clk, R : IN std_logic;
            index : IN std_logic_vector(5 downto 0);
            data_in : IN std_logic_vector(7 downto 0);
            wr : IN std_logic;
            cs : IN std_logic;
            data_out : OUT std_logic_vector(7 downto 0)
        );
    END COMPONENT;

    COMPONENT data_memory2 IS
        PORT(
            clk, R : IN std_logic;
            index : IN std_logic_vector(5 downto 0);
            data_in : IN std_logic_vector(7 downto 0);
            wr : IN std_logic;
            cs : IN std_logic;
            data_out : OUT std_logic_vector(7 downto 0)
        );
    END COMPONENT;

    COMPONENT data_memory3 IS
        PORT(
            clk, R : IN std_logic;
            index : IN std_logic_vector(5 downto 0);
            data_in : IN std_logic_vector(7 downto 0);
            wr : IN std_logic;
            cs : IN std_logic;
            data_out : OUT std_logic_vector(7 downto 0)
        );
    END COMPONENT;

    COMPONENT cmp IS
        PORT(
            input1, input2 : IN std_logic_vector(7 downto 0);
            result : OUT std_logic
        );
    END COMPONENT;

    COMPONENT cu IS
        PORT(
            clk, reset : IN std_logic;
            hit : IN std_logic;
            write, update : OUT std_logic
        );
    END COMPONENT;

    SIGNAL decoder_output : std_logic_vector(0 TO 3) := "0000";
    SIGNAL chip_select : std_logic_vector(0 TO 3) := "0000";
    SIGNAL wr_signal, update_signal : std_logic := '0';
    SIGNAL data_mem0_out, data_mem1_out, data_mem2_out, data_mem3_out : std_logic_vector(7 downto 0) := x"00";
    SIGNAL tag_mem_out : std_logic_vector(8 downto 0) := "000000000";
    SIGNAL hit_flag : std_logic := '0';
    SIGNAL mux_output : std_logic_vector(7 downto 0) := x"00";
    SIGNAL buffer_enable : std_logic := '0';

BEGIN

    decoder_inst : decoder PORT MAP(
        select_signal => address(1 downto 0),
        output_vector => decoder_output
    );

    chip_select(0) <= decoder_output(0) AND update_signal;
    chip_select(1) <= decoder_output(1) AND update_signal;
    chip_select(2) <= decoder_output(2) AND update_signal;
    chip_select(3) <= decoder_output(3) AND update_signal;

    data_memory0_inst : data_memory0 PORT MAP(
        clk => clk, R => reset,
        index => address(7 downto 2),
        data_in => x"00",
        wr => wr_signal,
        cs => chip_select(0),
        data_out => data_mem0_out
    );

    data_memory1_inst : data_memory1 PORT MAP(
        clk => clk, R => reset,
        index => address(7 downto 2),
        data_in => x"00",
        wr => wr_signal,
        cs => chip_select(1),
        data_out => data_mem1_out
    );

    data_memory2_inst : data_memory2 PORT MAP(
        clk => clk, R => reset,
        index => address(7 downto 2),
        data_in => x"00",
        wr => wr_signal,
        cs => chip_select(2),
        data_out => data_mem2_out
    );

    data_memory3_inst : data_memory3 PORT MAP(
        clk => clk, R => reset,
        index => address(7 downto 2),
        data_in => x"00",
        wr => wr_signal,
        cs => chip_select(3),
        data_out => data_mem3_out
    );

    tag_memory_inst : tag_memory PORT MAP(
        clk => clk, R => reset,
        index => address(7 downto 2),
        data_in => address(15 downto 8),
        wr => update_signal,
        cs => '1',
        data_out => tag_mem_out
    );

    comparator_inst : cmp PORT MAP(
        input1 => address(15 downto 8),
        input2 => tag_mem_out(7 downto 0),
        result => hit_flag
    );

    WITH address(1 downto 0) SELECT mux_output <=
        data_mem0_out WHEN "00",
        data_mem1_out WHEN "01",
        data_mem2_out WHEN "10",
        data_mem3_out WHEN OTHERS;

    buffer_enable <= hit_flag AND tag_mem_out(8);
    dout <= mux_output WHEN (buffer_enable = '1') ELSE "ZZZZZZZZ";

    control_unit_inst : cu PORT MAP(
        clk => clk, reset => reset,
        hit => hit_flag,
        write => wr_signal,
        update => update_signal
    );

    mem0_out <= data_mem0_out;
    mem1_out <= data_mem1_out;
    mem2_out <= data_mem2_out;
    mem3_out <= data_mem3_out;
    tag_out <= tag_mem_out(7 downto 0);
    hit_signal <= hit_flag;

END Structural;