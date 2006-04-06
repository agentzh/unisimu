use strict;
use warnings;
use FAST;

my $if = FAST::Struct::If->new('<L=1>', '[a]', '[b]');
my $while = FAST::Struct::While->new('<L>0>', $if);
my $seq = FAST::Struct::Seq->new('L:=1', $while);

$if->as_debug('if.dot');
$while->as_debug('while.dot');
$seq->as_debug('seq.dot');
