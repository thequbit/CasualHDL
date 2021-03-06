###State Machines###

 - Only single bits may come in and out of a state machine
   - This includes assigning values to buses and comparing buses.
   
An example would be resetting a counter on start, then waiting until that counter
reached a specific value, and then multiplying two values once the counter hits that
value.  There should be a process for counting that also does the reset and compare.
This would allow for the state machine to only every have to interface to std_logic
signals.  We will have four states:

  - idle
    - waits for i_start to be asserted high, then moves to wait_for_count
  - wait_for_count
    - waits here until the counter has reached i_count_max, then moves to multiply
  - multiply
    - performs the multiplication of i_a and i_b and puts the result in r_c
  - finished
    - asserts the r_dv signal, and returns to idle.
     
**Note** All synth numbers are with Vivado 2014.4 targeting the Avnet MicroZed 
7020 SoM.

This is ***WRONG***! DO NOT DO IT THIS WAY!  Here, we see that the counter and the
multiplier both within the state machine logic.  This is bad.  Don't do this.  This
causes lots of Flip Flops to be used to handle all of the state logic for each bit
of each of the large buses for s_count and, i_a, i_b, and r_c.  Your state machine
ends up being WAY bigger on chip that you would expect it to be, and it will run
much slower than it could, since there are more signals to route.

    
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.std_logic_unsigned.all;

    entity multiplier_bad is
        port (
            i_clk : in STD_LOGIC;
            i_reset : in STD_LOGIC;
            i_start : in STD_LOGIC;
            i_count_max : in STD_LOGIC_VECTOR(31 downto 0);
            i_a : in STD_LOGIC_VECTOR (31 downto 0);
            i_b : in STD_LOGIC_VECTOR (31 downto 0);
            o_c : out STD_LOGIC_VECTOR (63 downto 0);
            o_dv : out STD_LOGIC
        );
    end multiplier_bad;

    architecture Behavioral of multiplier_bad is

        type state_type is (
            idle,
            wait_for_count,
            multiply,
            finished
        );
        signal state : state_type := idle;

        signal s_count : std_logic_vector(31 downto 0) := (others => '0');
        signal r_c : std_logic_vector(63 downto 0) := (others => '0');
        signal r_dv : std_logic := '0';

    begin

        o_c <= r_c;
        o_dv <= r_dv;

        -- state machine process;
        process(i_clk)
        begin
            if (rising_edge( i_clk ) ) then
                if ( i_reset = '1' ) then
                    -- reset values
                    s_count <= (others => '0');
                    r_c <= (others => '0');
                    r_dv <= '0';
                else
                    -- defaults
                    s_count <= s_count;
                    r_c <= (others => '0'); -- output from multiply (SUPER BAD!)
                    r_dv <= '0'; -- output value for data valid, asserted high
        
                    -- state machine
                    case state is
                        when idle =>
                            s_count <= (others => '0'); -- BAD!
                            if ( i_start = '1' ) then
                                state <= wait_for_count;
                            end if;
                        when wait_for_count =>
                            s_count <= s_count + '1'; -- BAD!
                            if ( s_count = i_count_max ) then -- BAD!
                                state <= multiply;
                            end if;
                        when multiply =>
                            r_c <= i_a * i_b; -- SUPER BAD!
                            state <= finished;
                        when finished =>
                            r_dv <= '1';
                            state <= idle;
                        when others =>
                            null;
                    end case;
                end if;
            end if;
        end process;

    end Behavioral;

    
This ***BAD*** code produced this utalization with 32 bit inputs and 64 bit output:

    +-------------------------+------+-------+-----------+-------+
    |        Site Type        | Used | Fixed | Available | Util% |
    +-------------------------+------+-------+-----------+-------+
    | Slice LUTs*             |  141 |     0 |     17600 |  0.80 |
    |   LUT as Logic          |  141 |     0 |     17600 |  0.80 |
    |   LUT as Memory         |    0 |     0 |      6000 |  0.00 |
    | Slice Registers         |   99 |     0 |     35200 |  0.28 |
    |   Register as Flip Flop |   99 |     0 |     35200 |  0.28 |
    |   Register as Latch     |    0 |     0 |     35200 |  0.00 |
    | F7 Muxes                |    0 |     0 |      8800 |  0.00 |
    | F8 Muxes                |    0 |     0 |      4400 |  0.00 |
    +-------------------------+------+-------+-----------+-------+
    
    +----------------+------+-------+-----------+-------+
    |    Site Type   | Used | Fixed | Available | Util% |
    +----------------+------+-------+-----------+-------+
    | DSPs           |    4 |     0 |        80 |  5.00 |
    |   DSP48E1 only |    4 |       |           |       |
    +----------------+------+-------+-----------+-------+
    
    Worse reported time delay: 11.229ns, or 89.055MHz
     
Example VHDL state machine using only single bit signals:

Below is the way you should be doing your state machines.  Only single input and output
signals (bits) from the state machine.  This is more VHDL code, however results in less
Flip Flops and LUTs used, and thus a faster design.  Note that we are just multiplying
all of the time, with no gate.  This will produce a design that takes more power (since
the DSP48 primitive is always performing the multiplication), however it is a trade off
that is usually acceptable (an increase of a few mW in a multi-watt design should be 
lost in the noise ...
     
    
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.std_logic_unsigned.all;

    entity multiplier is
        port (
            i_clk : in STD_LOGIC;
            i_reset : in STD_LOGIC;
            i_start : in STD_LOGIC;
            i_count_max : in STD_LOGIC_VECTOR(31 downto 0);
            i_a : in STD_LOGIC_VECTOR (31 downto 0);
            i_b : in STD_LOGIC_VECTOR (31 downto 0);
            o_c : out STD_LOGIC_VECTOR (63 downto 0);
            o_dv : out STD_LOGIC
        );
    end multiplier;

    architecture Behavioral of multiplier is

        type state_type is (
            idle,
            wait_for_count,
            multiply,
            finished
        );
        signal state : state_type := idle;

        signal s_count : std_logic_vector(31 downto 0) := (others => '0');
        signal s_reset_count : std_logic := '0';
        signal s_done_counting : std_logic := '0';
        signal r_c : std_logic_vector(63 downto 0) := (others => '0');
        signal r_dv : std_logic := '0';
        
    begin

        o_c <= r_c;
        o_dv <= r_dv;

        process( i_clk ) 
        begin
            if ( rising_edge( i_clk ) ) then
                if ( i_reset = '1' ) then
                    -- reset values
                    s_count <= (others => '0');
                    s_done_counting <= '0';
                else
                   -- defaults
                    s_count <= s_count;
                    s_done_counting <= '0';
        
                    -- counter logic
                    if ( s_reset_count = '1' ) then
                        s_count <= (others => '0');
                    -- we do 1 less than the count because we are delayed by one from the state
                    -- machine logic.
                    elsif ( s_count = i_count_max - '1') then 
                        s_done_counting <= '1';
                    else
                        s_count <= s_count + '1';
                    end if;
                end if;
            end if;
        end process;

        process( i_clk ) 
        begin
            if ( rising_edge( i_clk ) ) then
                if ( i_reset = '1' ) then
                    
                    r_c <= (others => '0');
                    
                else
                    
                    -- there is no need to gat this operation, just
                    -- let it free run. (note: this will increase the owier
                    -- consuption of your design, if that is up concern
                    -- then gate it with a s_do_multiply signal)
                    r_c <= i_a * i_b;
                    
                end if;
            end if;
        end process;
        
        process( i_clk ) 
            begin
                if ( rising_edge( i_clk ) ) then
                    if ( i_reset = '1' ) then
                        
                        -- reset values
                        s_reset_count <= '0'; 
                        
                        r_dv <= '0'; 
                        
                    else
                        
                        -- defaults
                        s_reset_count <= '0'; 
                        
                        r_dv <= '0'; 
                        
                        -- state machine
                        case state is
                            when idle =>
                                if ( i_start = '1' ) then
                                    state <= wait_for_count;
                                end if;
                            when wait_for_count =>
                                if ( s_done_counting = '1' ) then
                                    state <= multiply;
                                end if;
                            when multiply =>
                                state <= finished;
                            when finished =>
                                r_dv <= '1';
                                state <= idle;
                            when others => 
                                null;
                        end case;
                        
                    end if;
                end if;
            end process;

    end Behavioral;

    
This "correct" code produced this utalization with the same 32 bit inputs and 64 bit outputs:

    +-------------------------+------+-------+-----------+-------+
    |        Site Type        | Used | Fixed | Available | Util% |
    +-------------------------+------+-------+-----------+-------+
    | Slice LUTs*             |  126 |     0 |     17600 |  0.71 |
    |   LUT as Logic          |  126 |     0 |     17600 |  0.71 |
    |   LUT as Memory         |    0 |     0 |      6000 |  0.00 |
    | Slice Registers         |   70 |     0 |     35200 |  0.19 |
    |   Register as Flip Flop |   70 |     0 |     35200 |  0.19 |
    |   Register as Latch     |    0 |     0 |     35200 |  0.00 |
    | F7 Muxes                |    0 |     0 |      8800 |  0.00 |
    | F8 Muxes                |    0 |     0 |      4400 |  0.00 |
    +-------------------------+------+-------+-----------+-------+
    
    +----------------+------+-------+-----------+-------+
    |    Site Type   | Used | Fixed | Available | Util% |
    +----------------+------+-------+-----------+-------+
    | DSPs           |    4 |     0 |        80 |  5.00 |
    |   DSP48E1 only |    4 |       |           |       |
    +----------------+------+-------+-----------+-------+
    
    Worse reported time delay: 7.02ns, or 142.450MHz
    
The first example will take up much more space in the FPGA fabric, and will run
slower, that is the maximum clock rate will be much lower.   The second will
produce a design that is smaller and faster.  Note, though, that it is more code.
Just because there is more VHDL, does not mean that the design itself will not 
be smaller.

The numbers for comparison:

    +--------------+---------+--------+----------------+
    |              |    BAD  |    GOOD  |  Difference  |
    +--------------+---------+----------+--------------+
    |  LUTs        |    141  |     126  |      -10.6%  |    
    |  Registers   |     99  |      70  |      -29.3%  |
    |  Clock Rate  |  89Mhz  |  142Mhz  |      +59.6%  |
    +--------------+---------+----------+--------------+
    
Moral of the story: Don't have anything more than single bits coming in and out
of your state machines!
