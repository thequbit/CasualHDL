# CasualHDL
Casual HDL is an Extension Language for VHDL

## Background ##

CasualHDL, or Casual Hardware Description Language, is an extension language
for VHDL.  CasualHDL allows HDL designers to focus on functionality of their
code rather than syntax and verbosity, while ensuring best design practises
for synchronous and asynchronous operations.

The output of the CasualHDL processor produces VHDL code that implements 
design methodologies to ensure extremely high speed ( low time between 
registers ), and small ( low LUT and FF count ) designs.  The output 
VHDL structors have been developed based on years of experience,
experimentation and research into FPGA design and VHDL inference.

VHDL is a strongly typed, verbose language.  VHDL’s strongly typed syntax 
and verbosity is often looked at as a blessing and a curse.  It can be 
convenient when debugging since little is left for interpretation, however 
writen the code can be frustrating and lengthy.  CasualHDL looks to remove 
this frustration by abstracting some of this verbosity from the designer, 
and helps ensure the highest possible output code quality.

### Theory of Operation ###

VHDL is inherently hierarchical, and thus CasualHDL strives to embrace 
that method of organizing functionality this same way.  In VHDL, each 
functional block of code is referred to as an entity.  Each CasualHDL
functional block is also called an entity.  Entities are interfaced together 
to implement the full intended functionality of the design.

An example of a hierarchical design for a PWM controller with an external
trigger/enable:

    | - top_pwm_ctrl.chdl
        | - clock_enable.chdl [ clock_enable_generator ]
            | - counter.chdl [ ce_pulse_counter ]
        | - pwm.chdl [ pwm_output_generator ]
            | - counter.chdl [ high_time_counter ]
            | - counter.chdl [ low_time_counter ]
        | - ext_triger.vhdl [ ext_trigger_control ]

CasualHDL uses indentations to define control structures (similar to SASS and 
Python).These structures are similar to those found in VHDL (entity, if, elif, 
else, case, when, etc.), however there are some that are ‘missing’ due to 
abstraction (architecture for example).

### Simple Counter Example ###

An example of a simple 32 bit counter in CasualHDL:


     1  entity counter:
     2 
     3      clk clock
     5      reset_count logic
     6      count logic(31,0)
     7
     8  arch:
     9
    10      def counter_decode:
    11  
    12          reset:
    13              count = 0
    14 
    15          sync clk:
    16              if reset_count:
    17                  count = 0
    18              else:
    19                  count += 1
    20

Let’s look at this line-by-line:

#### CasualHDL Entity ####

1: `entity counter:` - this line defines our entity name.  This is the name of the 
functional block of code, and will be used when ‘importing’ this entity into other 
entities when you want to use it.

The next three lines will define the external port of the entity.  These are the
ports that are accessible by other entities when this entity is imported into them.

3: `clk clock` - this defines a port called `clk` of type `clock`.  Since there are 
synchronous parts of this entity, we’ll need a clock to clock those flip flops.  The
`clock` port type is the same as the `ieee.std_logic_1164.std_logic` signal type.  
Note: if a sync control instruction is used with a port that is not defined as a clock,
an error will be thrown.

4. `reset_count logic` - this defines a port called `reset_count` and is of type `logic`.
The `logic` port type maps directly to the `ieee.std_logic_1164.std_logic` signal type.  

5. `count logic(31,0)` - this defines a port called `count` and is of type `logic(31,0)`.
The `logic(31,0)` port type maps to the `ieee.std_logic_1164.std_logic_vector` signal type,
with a width of 31 downto 0.  `count logic(32)` would have produced the same result.  
`count logic(0,31)` would have produced `std_logic_vector(0 to 31)`.

The CasualHDL code on lines 1 - 6 will generate the following VHDL code:

     1  entity counter is
     2  port (
     3      i_clk : in std_logic;
     4      i_reset : in std_logic;
     5
     6      i_reset_count : in std_logic;
     7      o_count : out std_logic_vector(31 downto 0)
     8  );
    end counter;

We can see that there are pre-fixes appended to the signals defined in the port statement, 
and some signals are inputs and some are outputs.  These decisions are made based on how 
the signals are used within the `arch` definition of the entity (more on that below). 
Additionally, there is a new signal that has been added that was not explicitly in the 
CasualHDL `entity`: `i_reset`.  This signal is the signal that will be used with the 
CasualHDL `reset` control structure.  By default the `reset` structure is synchronous, 
however processor directives can be included within the CasualHDL source file to make 
the reset asynchronous.

#### CasualHDL Architecture ####

8. `arch:` - This is the beginning of the description of our entities functionality

11. `` - This is where we would declare signals used within our entity description.  
Since we do not have any for this simple design, there are no definitions here.

10. `def counter_decode:` - This defines a specific block of code, with the label 
`counter_decode`. The entity definition is built up of a number of these `def` statements.  
Think of these as very similar to a `process` in VHDL.

Each `def` code block requires a `reset` structure and a `sync` or `async` structure.  
In the case of `sync` a clock must also be defined as what the code should be synchronous
to.

12. `reset:` - This is the reset control structure for the `def`.  It defines what the
values of the signals and ports referenced within the def should be.  Note: all signals
used within the `def` MUST have a reset value defined.

13. `count = 0` - this assigns the value of `0` to `count`.  This happens when reset is
asserted.  Note: unlike with VHDL where specific syntax must be used for different logic
types (single quote for std_logic and double quote for std_logic_vector, CasualHDL makes
assumptions for you and does not require quotes at all.  Since `count` was defined as type
`logic(31,0)` the resulting VHDL code will be:

    count<= X”00000000”;

15. `syncclk:` - This defines our synchronous functionality for the `def`.  Note only 
`sync` OR `async` can be used within a `def` structure, not both.  A separate `def` 
structure should be used if both `sync` and `async` is needed.  sync commands cause the 
included logic to be synchronous to the rising edge of the specified clock.  If the `def`
structure should be synchronous to the falling edge of `clk`, then the following syntax 
should be used:

    sync !clk:

16. `if reset_count:` - This is the `if` control structure, and is testing to see if
`reset_count` is evaluated to `true`.  Since `reset_count` was defined as type `logic`,
CasualHDL will produce the following VHDL code based on this line:

    if ( i_reset_count = ‘1’ ) then

CasualHDL knows that `reset_count` only has one definition that evaluates to `true` and 
that is being a value of `1`.

17. `count = 0` - This sets `count` to a value of `0`.  Since `count` was defined as type
`logic`, CasualHDL will produce the appropriate VHDL code.  Additionally, since this is 
within the `if` control structure defined on line 16, this will only happen if `i_reset_count`
equals `1`.

18. ‘else:’ - Just like with VHDL, an `if` control structure may contain a single `else` 
control structure.  Also, an `if` control structure may contain any number of `elsif` 
control structures.

19. ‘count += 1` - This increments the current value of `count` by `1`.  Since `count` 
was defined as type `logic(31,0)`, CasualHDL will produce the following VHDL code:

    count <= count + ‘1’;

The intended operation here is that, unless counter_reset is asserted, the count will 
increment on every rising edge of the clock.

The CasualHDL from lines 8 - 19 produce the following VHDL code:

     1  architecture behavioral of counter is
     2  
     3      signal r_o_count : std_logic_vector(31 downto 0) := (others => '0');
     4
     5  begin
     6
     7      o_count <= r_o_count;
     8
     9      process( i_clk )
    10      begin
    11          if ( rising_edge( i_clk ) ) then
    12              if ( i_reset = '1' ) then
    13                  r_o_count <= (others => '0');
    14              else
    15                  r_o_count <= r_o_count;
    16                  if ( i_reset_count = '1') then
    17                      r_o_count <= (others => '0');
    18                  else
    19                      r_o_count <= r_o_count + '1';
    20                  end if;
    21              end if;
    22          end if;
    23      end process;
    24  
    25  end behavioral;
    26

There are a few things to note here:

 - On line 3 we see that a signal is created to represent the register for the output 
 `o_count` port.  This is done so a default value can be applied to the output to ensure 
 that a latch isn’t generated within the FPGA.
 - On line 7 the r_o_count register is assigned to the external port, forwarding the 
 value out of the entity.
 - On line 11 the entire process is synchronous to the rising edge of the clock.
 - On line 12 there is a synchronous, asserted high reset signal.
 - On line 15 a default value of its previous value is assigned to r_o_clock.  This helps
 prevent the tools from inferring a latch rather than a register.

### Symbols and Syntax ###

There are a limit number of symbols used within CasualHDL to ensure simplicity:

**Control Structures**

 - `entity`
   - entity
 - `arch`
   - architecture
 - `def`
   - process
 - `reset`
   - reset
 - `sync`
   - synchronous
 - `async`
   -- asynchronous
 - `if`
   - if
 - `case`
   - case
 - `when`
   - when

**signal types**
  - `clock`
    - std_logic
  - `logic`
    - std_logic
  - `logic(x,y)
    - std_logic_vector()
  - `enum(...)`
    - type
  
** symbols **
 - `=`
   - compare or assign
 - '!='
   - compare 
 - '<'
 - '<='
 - '>'
 - '>='
 - '!'
   - not
 
### More Advanced, Nested Design ###

The following CasualHDL code produces a pwm controller:

     1  -- import our counter entity
     2  import counter
     3
     4  -- include clock and reset signals
     5  set use_clock true
     6  set use_reset true
     7
     8  -- pwm entity
     9  entity pwm:
    10   
    12      -- start the pwm operation
    13      start logic
    14
    15      -- ce clock pulse width
    16      clock_divide logic(31,0)
    17    
    18      -- time to stay high and low
    19      high_count logic(31,0)
    20      low_count logic(31,0)
    21
    22      -- output pulse
    23      pulse logic
    24    
    25  arch behavioral:
    26
    27      -- state machine states
    28      state enum(
    29        idle,
    30        high_pulse,
    31        low_pulse
    32      )
    33    
    34      -- create high and low pulse width counters
    35      high_count counter
    36      low_count counter
    37    
    38      -- ce counter
    39      ce_count counter
    40    
    41    -- create clock enable pulse
    42    def ce_decode:
    43    
    44        reset:
    45            ce = 0
    46            ce_count.reset_count = 1
    47            
    48        sync clk:
    49            ce_count.reset_count = 0
    50            ce = 0
    51            if ce_count.count = clock_divide:
    52                ce = 1
    53                ce_count.reset_count = 1
    54
    55    
    56    -- state machine decode
    57    def state_decode:
    58        
    59        reset:
    60            state = idle
    61            o_pulse = 0
    62
    63        sync clk:
    64            if ce = 1:
    65                
    66                -- defaults
    67                high_count.reset_count = 0
    68                low_count.reset_count = 0
    69                
    70                case state:
    71                
    72                    -- wait for start
    73                    when idle:
    74                        high_count.reset_count = 1
    75                        low_count.reset_count = 1
    76                      if i_start:
    77                            state = high_pulse
    78                        o_pulse = 0
    79                            
    80                    -- high pulse output
    81                    when high_pulse:
    82                        if high_count.count = high_count:
    83                            state = low_pulse
    84                        o_pulse = 1
    85                        
    86                    -- low pulse output
    87                    when low_pulse:
    88                        if low_count.count = low_count:
    89                            state = idle;
    90                        o_pulse = 0
    91                        

The above code produces the following VHDL code:


      1 -- pwm.vhd
      2 
      3 library ieee;
      4 use ieee.std_logic_1164.all;
      5 use ieee.numeric_std.all;
      6 use ieee.std_logic_unsigned.all;
      7   
      8 entity pwm is
      9 port(
     10     i_clk : in std_logic;
     11     i_reset : in std_logic;
     12     
     13     -- start the pwm operation
     14     i_start : in std_logic;
     15       
     16     -- ce clock pulse width
     17     i_clock_divide : in std_logic_vector(31 downto 0);
     18       
     19     -- time to stay high and low
     20     i_high_count : in std_logic_vector(31 downto 0);
     21     i_low_count : in std_logic_vector(31 downto 0);
     22         
     23      -- output pulse
     24     o_pulse : out std_logic
     25 );
     26 end pwm;
     27
     28 architecture behavioral of pwm is
     29     
     30     type t_state_type is (
     31         idle,
     32         high_pulse,
     33         low_pulse
     34     );
     35     signal s_state : t_state_type := idle;
     36     
     37     -- o_pulse output register
     38     signal s_o_pulse : std_logic := '0';
     39     
     40     component counter is 
     41     port (
     42         i_clk : in std_logic;
     43         i_reset : in std_logic;
     44         i_reset_count : in std_logic;
     45         o_count : out std_logic_vector(31 downto 0)
     46     );
     47     end component;
     48     
     49     signal s_high_count_counter_reset_count : std_logic;
     50     signal s_high_count_counter_count : std_logic_vector(31 downto 0);
     51     
     52     signal s_low_count_counter_reset_count : std_logic;
     53     signal s_low_count_counter_count : std_logic_vector(31 downto 0);
     54  
     55     signal s_ce_count_counter_reset_count : std_logic;
     56     signal s_ce_count_counter_count : std_logic_vector(31 downto 0);
     57      
     58     signal s_ce : std_logic;
     59
     60     signal s_high_count_counter_count_equals_i_high_count : std_logic;
     61     signal s_low_count_counter_count_equals_i_low_count : std_logic;
     62
     63 begin
     64
     65     -- output register forward
     66     o_pulse <= s_o_pulse;
     67     
     68     -- high_count counter
     69     inst_high_count_counter : counter
     70     port map (
     71         i_clk => i_clk,
     72         i_reset => i_reset,
     73         i_reset_count => s_high_count_counter_reset_count,
     74         o_count => s_high_count_counter_count
     75     );
     76     
     77     -- low_count counter
     78     inst_low_count_counter : counter
     79     port map (
     80         i_clk => i_clk,
     81         i_reset => i_reset,
     82         i_reset_count => s_low_count_counter_reset_count,
     83         o_count => s_low_count_counter_count
     84     );
     85     
     86     -- ce_count counter
     87     inst_ce_count_counter : counter
     88     port map (
     89         i_clk => i_clk,
     90         i_reset => i_reset,
     91         i_reset_count => s_ce_count_counter_reset_count,
     92         o_count => s_ce_count_counter_count
     93     );
     94
     95     -- create clock enable pulse
     96     process( i_clk )
     97     begin
     98         if ( rising_edge( i_clk ) ) then
     99             if ( i_reset = '1' ) then
    100                 s_ce <= '0';
    101                 s_ce_count_counter_reset_count <= '1';
    102             else
    103            
    104                 -- defaults
    105                 s_ce_count_counter_reset_count <= '0';
    106                 s_ce <= '0';
    107         
    108                 if ( s_ce_count_counter_count = i_clock_divide ) then
    109                     s_ce <= '1';
    110                     s_ce_count_counter_reset_count <= '1';
    111                 else
    112                     s_ce <= '0';
    113                 end if;
    114             end if;
    115         end if;
    116     end process;
    117
    118     -- if high_count.count = high_count:
    119     s_high_count_counter_count_equals_i_high_count <= '1' when s_high_count_counter_count = i_high_count else '0';
    120      
    121     -- if low_count.count = low_count:
    122     s_low_count_counter_count_equals_i_low_count <= '1' when s_low_count_counter_count = i_low_count else '0';
    123     
    124     -- state machine decode
    125     process( i_clk )
    126     begin
    127         if ( rising_edge( i_clk ) ) then
    128             if ( i_reset = '1' ) then
    129                 s_state <= idle;
    130                 s_o_pulse <= '0';
    131                 s_high_count_counter_reset_count <= '0';
    132                 s_low_count_counter_reset_count <= '0';
    133             else
    134                 
    135                 -- external to clock enable defaults
    136                 s_state <= s_state;
    137                 s_o_pulse <= s_o_pulse;
    138                 s_high_count_counter_reset_count <= s_high_count_counter_reset_count;
    139                 s_low_count_counter_reset_count <= s_low_count_counter_reset_count
    140         
    141                 if ( s_ce = '1' ) then
    142             
    143                    -- internal to clock enable defaults
    144                     s_high_count_counter_reset_count <= '0';
    145                     s_low_count_counter_reset_count <= '0';
    146                 
    147                    -- state machine
    148                     case s_state is
    149                             
    150                         -- wait for start
    151                         when idle =>
    152                          s_high_count_counter_reset_count <= '1';
    153                             s_low_count_counter_reset_count <= '1';
    154                             if ( i_start = '1' ) then
    155                                 s_state <= high_pulse;
    156                             end if;
    157                             s_o_pulse <= '0';
    158                     
    159                        -- high pulse output
    160                         when high_pulse =>
    161                             if ( s_high_count_counter_count_equals_i_high_count = '1' )  then
    162                                 s_state <= low_pulse;
    163                             end if;
    164                             s_o_pulse <= '1';
    165                         
    166                        -- low pulse output
    167                         when low_pulse =>
    168                             if ( s_low_count_counter_count_equals_i_low_count = '1' ) then
    169                                 s_state <= high_pulse;
    170                             end if;
    171                             s_o_pulse <= '0';
    172                         when others => null;
    173                     end case;
    174            
    175                 end if;
    176         
    177             end if;
    178         end if;
    179     end process;
    180
    181 end behavioral;
    
Note that only single bit signals are brough into the stae machine to ensure high speed operation.

## Conclusions ##

CasualHDL is a work in progress, however has shown to be promissing already in the simplicity of generating 
large HDL designs, and the quality of the code produced.
