use strict;
use warnings;

use Test::More tests => 5;
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
$disp->run(5);
is $i, 5, 'counter worked as expected';
is $disp->now, 7, 'now is 7';
$disp->reset;
is $disp->time_of_next, undef, 'queue is empty';
is $disp->now, 0, 'clock reset';
#warn "YO!";
#print $i;
