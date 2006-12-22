use strict;
use warnings;

use Test::More tests => 8;
BEGIN { use_ok('Sim::Dispatcher'); }

my $disp = 'Sim::Dispatcher';

#warn "HERE!";
my $i = 0;
my $hdl;
$hdl = sub {
    #warn "i = $i\n";
    $i++;
    $disp->schedule($disp->now + 1 => $hdl);
};
#warn "THERE!";
$disp->schedule(3 => $hdl);
#warn "HI!";
$disp->run(fires => 5);
is $i, 5, 'counter worked as expected';
is $disp->now, 7, 'now is 7';
$disp->reset;
is $disp->time_of_next, undef, 'queue is empty';
is $disp->now, 0, 'clock reset';
#warn "YO!";
#print $i;

$disp->reset;
$i = 0;
is $disp->now, 0, 'clock reset';
is $disp->time_of_next, undef, 'queue reset';
$disp->schedule(3 => $hdl);
$disp->run(duration => 5);
is $i, 3, 'counter works for duration 5';
