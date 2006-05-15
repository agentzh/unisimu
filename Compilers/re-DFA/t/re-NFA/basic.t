# basic.t

use strict;
use warnings;
use File::Compare 'compare';

use Test::More tests => 5 * 2 + 1;
BEGIN { use_ok('re::NFA'); }

*gen = \&re::NFA::translate;

my $i = 0;

test('a');
test('ab');
test('a*');
test('a|b');
test('(a|b)*(aa|bb)(a|b)*');

sub test {
    my $src = shift;

    my $imfile  = 't/re-NFA/nfa'.++$i.'.png';
    my $stdfile = 't/re-NFA/~nfa'.$i.'.png';
    unlink $imfile if -f $imfile;
    gen($src, $imfile);
    ok -f $imfile, "'$src' - comp $imfile $stdfile";
    is compare($imfile, $stdfile), 0, "'$src' - comp $imfile $stdfile";
}
