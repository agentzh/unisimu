use strict;
use warnings;

use Test::More tests => 7;
BEGIN { use_ok('Sim::Dispatcher'); }

use Sim::Clock;

my $disp = new Sim::Dispatcher(new Sim::Clock);
ok $disp, 'obj ok';
isa_ok $disp, 'Sim::Dispatcher';

#warn "HERE!";
my $i = 0;
my $hdl;
$hdl = sub {
    #warn "i = $i\n";
    $i++;
    $disp->schedule($disp->clock->now + 1 => $hdl);
};
#warn "THERE!";
$disp->schedule(3 => $hdl);
#warn "HI!";
$disp->run(5);
is $i, 5, 'counter worked as expected';
is $disp->clock->now, 7, 'now is 7';
$disp->reset;
is $disp->time_of_next, undef, 'queue is empty';
is $disp->clock->now, 0, 'clock reset';
#warn "YO!";
#print $i;
