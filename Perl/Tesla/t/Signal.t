#: Signal.t
#: Test script for Signal.pm
#: Tesla v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-11-11

use strict;
use warnings;
use Test::More tests => 20;
BEGIN { use_ok('Signal'); }

my $sig = Signal->new;
ok( defined $sig );
is( ref($sig), 'Signal' );

is( "$sig", $sig->{_name} );
is( "$sig", $sig->name );

ok(eq_array( $sig->{_dests}, [] ));
ok(eq_array( [$sig->dests], [] ));

is( $sig->{_value}, 'U' );
is( $sig->value, 'U' );

ok(eq_array( $sig->{_hist}, [] ));
ok(eq_array( [$sig->hist], [] ));

$sig->name( 'SIG' );
is( $sig->{_name}, 'SIG' );
is( $sig->name, 'SIG' );

$sig->dests(['B','AD']);
ok(eq_array( $sig->{_dests}, ['B','AD'] ));
ok(eq_array( [$sig->dests], ['B','AD'] ));

$sig = Signal->new;
$sig->hist([0,1,1,1,1,2,1,3]);
is( $sig->histp, '1@1' );
$sig->hist([0,1,1,2,1,3,0,4]);
is( $sig->histp, '0@1,1@2,0@4' );
$sig->hist([0,1,0,1,0,2]);
is( $sig->histp, '0@1' );

$sig->add_dest('A');
$sig->add_dest('B');
$sig->add_dest('C',"D");
ok(eq_array( [$sig->dests], ['A','B','C','D'] ));

$sig->force;
is( $sig->value, 'U' );

0;

