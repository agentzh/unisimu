use strict;
use warnings;

use Jifty::Test tests => 7;
use_ok('Qooqle::Model::MessageCollection');

is conv('cherry'), '[cherry]';
is conv('cherry is    here'), '[cherry] [is] [here]';
is conv("  cherry's here!  "), "[cherry's] [here!]";
is conv(q{i said, 'cherry\'s here!'}), "[i] [said,] [cherry's here!]";
is conv(q{i said, "cherry's here!"}), "[i] [said,] [cherry's here!]";
is conv(q{i said, "print \"hello, world!\n\";"}),
    q{[i] [said,] [print "hello, world!\n";]};

sub conv {
    my $s = shift;
    my $list = Qooqle::Model::MessageCollection->split_keys($s);
    join ' ', map { "[$_]" } @$list;
}
