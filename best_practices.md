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
     
This is ***WRONG***! DO NOT DO IT THIS WAY!  Here, we see that the counter and the
multiplier both within the state machine logic.  This is bad.  Don't do this.  This
causes lots of Flip Flops to be used to handle all of the state logic for each bit
of each of the large busses for s_count and, i_a, i_b, and r_c.  Your state machine
ends up being WAY bigger on chip that you would expect it to be, and it will run
much slower than it could, since there are more signals to route.

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
                    when idle
                        s_count <= (others => '0'); -- BAD!
                        if ( i_start = '1' ) then
                            state <= wait_for_count;
                        end if;
                    when wait_for_count
                        s_count <= s_count + '1'; -- BAD!
                        if ( s_count == i_count_max ) then -- BAD!
                            state <= multiply
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
     
Example VHDL state machine using only single bit signals:

Below is the way you should be doing your state machines.  Only single input and output
signals (bits) from the state machine.  This is more VHDL code, however results in less
Flip Flots and LUTs used, and thus a faster design.  Note that we are just multiplying
all of the time, with no gate.  This will produce a design that takes more power (since
the DSP48 primative is always performing the mulitiplication), however it is a trade off
that is usually acceptable (an increase of a few mW in a multi-watt design should be 
lost in the noise ...
     
     -- state machine process
    process( i_clk ) 
    begin
        if ( rising_edge( i_clk ) ) then
            if ( i_reset = '1' ) then
                -- reset values
                s_reset_count <= '0';
                s_do_multiply <= '0';
                r_dv <= '0';
            else
                -- defaults
                s_reset_count <= '0'; -- resets the counter if asserted high
                s_do_multiply <= '0'; -- performs the multiplication if asserted high
                r_dv <= '0'; -- output value for data valid, asserted high
                
                -- state machine
                case state is
                    when idle =>
                        s_reset_count <= '1';
                        if ( i_start = '1' ) then
                            state <= wait_for_count;
                        end if;
                    when wait_for_count =>
                        if ( s_done_counting = '1' ) then
                            state <= multiply;
                        end if;
                    when multiply =>
                        -- note we actually don't do anything here, it
                        -- is a dummy state because the multiply takes 2
                        -- clock cycles ( multiply + finished )
                        state <= finished;
                    when finished =>
                        r_dv <= '1';
                        state <= idle;
                    when others =>
                        null; -- we cover all our cases.
                end case;
            end if;
        end if;
    end process;
    
    -- counter process
    process( i_clk ) 
    begin
        if ( rising_edge( i_clk ) ) then
            if ( i_reset = '1' ) then
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
    
    -- multiplier process
    process( i_clk ) 
    begin
        if ( rising_edge( i_clk ) ) then
            if ( i_reset = '1' ) then
                r_c <= (others => '0');
            else
                -- always be multiplying.  Yes, it will cost you power, but
                -- your design will run MUCH faster.  the DSP48 blocks
                -- in the FPGA fabric don't like to be wrapped in too
                -- much logic.
                r_c <= i_a * i_b;
            end if;
        end if;
    end process;
The first example will take up much more space in the FPGA fabric, and will run
slower, that is the maximum clock rate will be much lower.   The second will
produce a design that is smaller and faster.  Note, though, that it is more code.
Just because there is more VHDL, does not mean that the design itself will not 
be smaller.
