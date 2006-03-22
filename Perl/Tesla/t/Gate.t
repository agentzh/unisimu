#: Gate.t
#: Test script for Gate.pm
#: Tesla v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-11-14

use strict;
use warnings;

use Test::More tests => 228;
BEGIN { use_ok('Gate') }

# ----------------------
# Test the Gate class:
# ----------------------
my $gate = Gate->new;
ok( defined $gate );
is( ref($gate), 'Gate' );

ok(eq_array( $gate->{_inputs}, [] ));
ok(eq_array( [$gate->inputs], [] ));

ok(!defined $gate->{_output});
ok(!defined $gate->{output});

is( $gate->{_delay}, 0 );
is( $gate->delay, 0 );

$gate->inputs(1,2,3);
ok(eq_array( $gate->{_inputs}, [1,2,3] ));
ok(eq_array( [$gate->inputs], [1,2,3] ));

$gate->output('A');
is( $gate->{_output}, 'A' );
is( $gate->output, 'A' );

$gate->delay(5.3);
is( $gate->{_delay}, 5.3 );
is( $gate->delay, 5.3 );

$gate = Gate->new(2.6);
ok( defined $gate );
is( ref($gate), 'Gate' );
ok(eq_array( $gate->{_inputs}, [] ));
ok( !defined $gate->{_output} );
is( $gate->{_delay}, 2.6 );

# ---------------------
# Test the AND class:
# ---------------------
$gate = AND->new;
ok( defined $gate );
is( ref($gate), 'AND' );

ok(eq_array( $gate->{_inputs}, [] ));
ok(eq_array( [$gate->inputs], [] ));

ok(!defined $gate->{_output});
ok(!defined $gate->{output});

is( $gate->{_delay}, 0 );
is( $gate->delay, 0 );

$gate->inputs(1,2,3);
ok(eq_array( $gate->{_inputs}, [1,2,3] ));
ok(eq_array( [$gate->inputs], [1,2,3] ));

$gate->output('A');
is( $gate->{_output}, 'A' );
is( $gate->output, 'A' );

$gate->delay(5.3);
is( $gate->{_delay}, 5.3 );
is( $gate->delay, 5.3 );

$gate = AND->new(2.6);
ok( defined $gate );
is( ref($gate), 'AND' );
ok( $gate->isa('Gate') );
ok(eq_array( $gate->{_inputs}, [] ));
ok( !defined $gate->{_output} );
is( $gate->{_delay}, 2.6 );

$AND::delay = 2.15;

$gate = AND->new;
is( $gate->{_delay}, 2.15 );
is( $gate->delay, 2.15 );

$gate = AND->new(13);
is( $gate->{_delay}, 13 );
is( $gate->delay, 13 );

$AND::delay = 0;

is( AND->func(0,1), 0 );
is( AND->func(1,1,1), 1 );
is( AND->func(1,1,0), 0 );
is( AND->func(0,0), 0 );
is( AND->func(1,0), 0 );

my @ok_vals = (
    [0,0,0] => 0,
    [1,1] => 1,
    [0,1,'U','X','Z'] => '0',
    ['Z','Z'] => 'Z',
    ['U','X' ] => 'U',
    ['1','1','Z'] => '1',
    [0,'Z'] => '0',
    ['U','X'] => 'U',
    [1,1,'U'] => 'U',
    [1,1,'X'] => 'X',
    [1,0,'X'] => '0',
);

while (@ok_vals) {
    my $rargs = shift @ok_vals;
    my $res = shift @ok_vals;
    is( AND->func(@$rargs), $res );
}

# ---------------------
# Test the OR class:
# ---------------------
$gate = OR->new;
ok( defined $gate );
is( ref($gate), 'OR' );

ok(eq_array( $gate->{_inputs}, [] ));
ok(eq_array( [$gate->inputs], [] ));

ok(!defined $gate->{_output});
ok(!defined $gate->{output});

is( $gate->{_delay}, 0 );
is( $gate->delay, 0 );

$gate->inputs(1,2,3);
ok(eq_array( $gate->{_inputs}, [1,2,3] ));
ok(eq_array( [$gate->inputs], [1,2,3] ));

$gate->output('A');
is( $gate->{_output}, 'A' );
is( $gate->output, 'A' );

$gate->delay(5.3);
is( $gate->{_delay}, 5.3 );
is( $gate->delay, 5.3 );

$gate = OR->new(2.6);
ok( defined $gate );
is( ref($gate), 'OR' );
ok( $gate->isa('Gate') );
ok(eq_array( $gate->{_inputs}, [] ));
ok( !defined $gate->{_output} );
is( $gate->{_delay}, 2.6 );

$OR::delay = 2.15;

$gate = OR->new;
is( $gate->{_delay}, 2.15 );
is( $gate->delay, 2.15 );

$gate = OR->new(13);
is( $gate->{_delay}, 13 );
is( $gate->delay, 13 );

$OR::delay = 0;

is( OR->func(0,1), 1 );
is( OR->func(1,1,1), 1 );
is( OR->func(1,1,0), 1 );
is( OR->func(0,0), 0 );
is( OR->func(1,0), 1 );

@ok_vals = (
    [0,0,0] => 0,
    [1,1] => 1,
    [0,1,'U','X','Z'] => '1',
    ['Z','Z'] => 'Z',
    ['U','X' ] => 'U',
    ['1','1','Z'] => '1',
    [0,'Z'] => '0',
    ['U','X'] => 'U',
    [1,1,'U'] => '1',
    [1,1,'X'] => '1',
    [1,0,'X'] => '1',
);

while (@ok_vals) {
    my $rargs = shift @ok_vals;
    my $res = shift @ok_vals;
    is( OR->func(@$rargs), $res );
}

# ---------------------
# Test the NOT class:
# ---------------------
$gate = NOT->new;
ok( defined $gate );
is( ref($gate), 'NOT' );

ok(eq_array( $gate->{_inputs}, [] ));
ok(eq_array( [$gate->inputs], [] ));

ok(!defined $gate->{_output});
ok(!defined $gate->{output});

is( $gate->{_delay}, 0 );
is( $gate->delay, 0 );

$gate->inputs('S');
ok(eq_array( $gate->{_inputs}, ['S'] ));
ok(eq_array( [$gate->inputs], ['S'] ));

$gate->output('A');
is( $gate->{_output}, 'A' );
is( $gate->output, 'A' );

$gate->delay(5.3);
is( $gate->{_delay}, 5.3 );
is( $gate->delay, 5.3 );

$gate = NOT->new(2.6);
ok( defined $gate );
is( ref($gate), 'NOT' );
ok( $gate->isa('Gate') );
ok(eq_array( $gate->{_inputs}, [] ));
ok( !defined $gate->{_output} );
is( $gate->{_delay}, 2.6 );

$NOT::delay = 2.15;

$gate = NOT->new;
is( $gate->{_delay}, 2.15 );
is( $gate->delay, 2.15 );

$gate = NOT->new(13);
is( $gate->{_delay}, 13 );
is( $gate->delay, 13 );

$NOT::delay = 0;

is( NOT->func(0), 1 );
is( NOT->func(1), 0 );
is( NOT->func('X'), 'X' );
is( NOT->func('U'), 'U' );
is( NOT->func('Z'), 'Z' );

# ---------------------
# Test the NAND class:
# ---------------------
$gate = NAND->new;
ok( defined $gate );
is( ref($gate), 'NAND' );

ok(eq_array( $gate->{_inputs}, [] ));
ok(eq_array( [$gate->inputs], [] ));

ok(!defined $gate->{_output});
ok(!defined $gate->{output});

is( $gate->{_delay}, 0 );
is( $gate->delay, 0 );

$gate->inputs(1,2,3);
ok(eq_array( $gate->{_inputs}, [1,2,3] ));
ok(eq_array( [$gate->inputs], [1,2,3] ));

$gate->output('A');
is( $gate->{_output}, 'A' );
is( $gate->output, 'A' );

$gate->delay(5.3);
is( $gate->{_delay}, 5.3 );
is( $gate->delay, 5.3 );

$gate = NAND->new(2.6);
ok( defined $gate );
is( ref($gate), 'NAND' );
ok( $gate->isa('Gate') );
ok(eq_array( $gate->{_inputs}, [] ));
ok( !defined $gate->{_output} );
is( $gate->{_delay}, 2.6 );

$NAND::delay = 2.15;

$gate = NAND->new;
is( $gate->{_delay}, 2.15 );
is( $gate->delay, 2.15 );

$gate = NAND->new(13);
is( $gate->{_delay}, 13 );
is( $gate->delay, 13 );

$NAND::delay = 0;

is( NAND->func(0,1), 1 );
is( NAND->func(1,1,1), 0 );
is( NAND->func(1,1,0), 1 );
is( NAND->func(0,0), 1 );
is( NAND->func(1,0), 1 );

# ---------------------
# Test the NOR class:
# ---------------------
my $class = 'NOR';
$gate = $class->new;
ok( defined $gate );
is( ref($gate), $class );

ok(eq_array( $gate->{_inputs}, [] ));
ok(eq_array( [$gate->inputs], [] ));

ok(!defined $gate->{_output});
ok(!defined $gate->{output});

is( $gate->{_delay}, 0 );
is( $gate->delay, 0 );

$gate->inputs(1,2,3);
ok(eq_array( $gate->{_inputs}, [1,2,3] ));
ok(eq_array( [$gate->inputs], [1,2,3] ));

$gate->output('A');
is( $gate->{_output}, 'A' );
is( $gate->output, 'A' );

$gate->delay(5.3);
is( $gate->{_delay}, 5.3 );
is( $gate->delay, 5.3 );

$gate = $class->new(2.6);
ok( defined $gate );
is( ref($gate), $class );
ok( $gate->isa('Gate') );
ok(eq_array( $gate->{_inputs}, [] ));
ok( !defined $gate->{_output} );
is( $gate->{_delay}, 2.6 );

{
    no strict 'refs';
    ${"${class}::delay"} = 2.15;
}

$gate = $class->new;
is( $gate->{_delay}, 2.15 );
is( $gate->delay, 2.15 );

$gate = $class->new(13);
is( $gate->{_delay}, 13 );
is( $gate->delay, 13 );

{
    no strict 'refs';
    ${"${class}::delay"} = 0;
}

{
    no strict 'refs';
    is( $class->func(0,1), 0 );
    is( $class->func(1,1,1), 0 );
    is( $class->func(1,1,0), 0 );
    is( $class->func(0,0), 1 );
    is( $class->func(1,0), 0 );
}

@ok_vals = (
    [0,'U'] => 'U',
    ['U',0] => 'U',
    [1,'U'] => '0',
    [1,0,'U'] => '0',
    [0,0,'U'] => 'U',
);

while (@ok_vals) {
    my $rargs = shift @ok_vals;
    my $res = shift @ok_vals;
    is( NOR->func(@$rargs), $res );
}

# ---------------------
# Test the XOR class:
# ---------------------
$class = 'XOR';
$gate = $class->new;
ok( defined $gate );
is( ref($gate), $class );

ok(eq_array( $gate->{_inputs}, [] ));
ok(eq_array( [$gate->inputs], [] ));

ok(!defined $gate->{_output});
ok(!defined $gate->{output});

is( $gate->{_delay}, 0 );
is( $gate->delay, 0 );

$gate->inputs(1,2,3);
ok(eq_array( $gate->{_inputs}, [1,2,3] ));
ok(eq_array( [$gate->inputs], [1,2,3] ));

$gate->output('A');
is( $gate->{_output}, 'A' );
is( $gate->output, 'A' );

$gate->delay(5.3);
is( $gate->{_delay}, 5.3 );
is( $gate->delay, 5.3 );

$gate = $class->new(2.6);
ok( defined $gate );
is( ref($gate), $class );
ok( $gate->isa('Gate') );
ok(eq_array( $gate->{_inputs}, [] ));
ok( !defined $gate->{_output} );
is( $gate->{_delay}, 2.6 );

{
    no strict 'refs';
    ${"${class}::delay"} = 2.15;
}

$gate = $class->new;
is( $gate->{_delay}, 2.15 );
is( $gate->delay, 2.15 );

$gate = $class->new(13);
is( $gate->{_delay}, 13 );
is( $gate->delay, 13 );

{
    no strict 'refs';
    ${"${class}::delay"} = 0;
}

{
    no strict 'refs';
    is( $class->func(0,1), 1 );
    is( $class->func(1,1,1), 1 );
    is( $class->func(1,1,0), 0 );
    is( $class->func(0,0), 0 );
    is( $class->func(1,0), 1 );
}

@ok_vals = (
    [0,0,0] => 0,
    [1,0,1] => 0,
    [0,0,1] => 1,
    [1,1,1] => 1,
    [0,1,1] => 0,
    [0,0,1,1] => 0,
    [1,0,1,0] => 0,
);

while (@ok_vals) {
    my $rargs = shift @ok_vals;
    my $res = shift @ok_vals;
    is( XOR->func(@$rargs), $res );
}

0;

