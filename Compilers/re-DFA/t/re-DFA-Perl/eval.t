# eval.t
# evaluate the regex to test re::DFA::Perl

use strict;
use warnings;

use Test::More 'no_plan';
BEGIN { use_ok('re::DFA::Perl'); }

*match = re::DFA::Perl->as_method('(a|ba)*');

is match('a'), 'a';
is match('ab'), 'a';
is match('b'), '';
is match('aaa'), 'aaa';
is match('aabaa'), 'aabaa';
is match('baaa'), 'baaa';
is match('bab'), 'ba';
is match(''), '';
is match('baaba'), 'baaba';

