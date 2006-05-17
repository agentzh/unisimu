# basic.t

use strict;
use warnings;
use File::Compare 'compare';

use Test::More tests => 8 * 2 + 1;
BEGIN { use_ok('re::DFA'); }

*gen = \&re::DFA::translate;

my $i = 0;

test('a');
test('ab');
test('a*');
test('a|b');
test('(a|ba)*');
test('(a|b)*(aa|bb)(a|b)*');
test('(a|)b*');
test('');

sub test {
    my $src = shift;

    my $imfile  = 't/re-DFA/dfa'.++$i.'.png';
    my $stdfile = 't/re-DFA/~dfa'.$i.'.png';
    unlink $imfile if -f $imfile;
    gen($src, $imfile);
    ok -f $imfile, "'$src' - comp $imfile $stdfile";
    is compare($imfile, $stdfile), 0, "'$src' - comp $imfile $stdfile";
}
