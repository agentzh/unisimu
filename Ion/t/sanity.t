use strict;
use warnings;

use File::Slurp;
use IPC::Run3;
use Test::Base;
plan tests => 2 * blocks();

system('xclips -I knowledge -c knowledge/reaction.xclp');

my $i = 0;
run {
    my $block = shift;
    my $name = $block->name;
    my $infile = "test-" . $i++ . ".xclp";
    write_file($infile, $block->input);
    my ($stdout, $stderr);
    run3 ['xclips.bat', '-I', 'knowledge', $infile, 'knowledge/reaction.clp'], \undef, \$stdout, \$stderr;
    warn $stderr if $stderr;
    is $?, 0, "$name - xclips ran successfully";
    is sort_ln($stdout), sort_ln($block->output), "$name - output okay";
};

sub sort_ln {
    join "\n", sort split /\n/, $_[0];
}

__DATA__

=== TEST 1:
--- input

ion(Ba, +2).
ion(Cl, -1).
ion(Cu, +2).
ion(Ag, +1).

--- output
{AgCl}



=== TEST 2:
XXX
--- input

ion(Ba, +2).
ion(Cl, -1).
ion(SO4, -2).
ion(Ag, +1).

--- output
{AgCl}
{BaSO4}

  

=== TEST 3:
--- input
include "sugar.xclp"

[Cu, +2], [PO4, -4].

--- output
{Cu2PO4}



=== TEST 4:
--- input
include "sugar.xclp"

[Fe, +3], [PO4, -4].

--- output
{Fe4(PO4)3}



=== TEST 5:
--- input
include "sugar.xclp"

[Na, +1], [K, +1], [NH4, +1], [PO4, -4].

--- output



=== TEST 6:
--- input
include "sugar.xclp"

[Mn, +4], [PO4, -4].

--- output
{MnPO4}



=== TEST 7:
--- input
include "sugar.xclp"

[X, +1], [PO4, -4].

--- output
{X4PO4}



=== TEST 8:
--- input
include "sugar.xclp"

[Mg, +2], [OH, -1].

--- output
{Mg(OH)2}



=== TEST 9: (A)
1、(06，四川)室温下，在强酸性和强碱性溶液中都不能大量共存的离子组是( )。
A、NH4+、Cu2+、Cl-、NO3-
B、K+、Na+、SO32-、S2-
C、 K+、Na+、SO42-、AlO2-
D、 Ba2+、Fe2+、NO3-、Br-
--- input
include "sugar.xclp"

[NH4, +1], [OH, -1], [Cu, +2], [Cl, -1], [NO3, -1].

--- output
{Cu(OH)2}
~NH3.H2O
