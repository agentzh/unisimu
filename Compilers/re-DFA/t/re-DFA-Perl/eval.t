# eval.t
# evaluate the regex to test re::DFA::Perl

use strict;
no warnings;

use Test::More tests => 55;
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

##

*match = re::DFA::Perl->as_method('(a|b)*(aa|bb)(a|b)*');

is match('aa'), 'aa';
is match('bb'), 'bb';
is match('aaa'), 'aaa';
is match('bbbb'), 'bbbb';
is match('bbaabb'), 'bbaabb';
ok !defined match('abab');
ok !defined match('a');
ok !defined match('b');
ok !defined match('ab');
ok !defined match('ba');
ok !defined match('');
is match('abbc'), 'abb';
ok !defined match('cabb');
is match('aaabbb'), 'aaabbb';

##

*match = re::DFA::Perl->as_method('(a|)b*');

is match('a'), 'a';
is match('aa'), 'a';
is match('aba'), 'ab';
is match('bbb'), 'bbb';
is match(''), '';
is match('cde'), '';
is match('abbb'), 'abbb';

##

*match = re::DFA::Perl->as_method('');

is match(''), '';
is match('abc'), '';

##

*match = re::DFA::Perl->as_method('a');

is match('a'), 'a';
is match('abc'), 'a';
ok !defined match(' abc');
ok !defined match('b');
ok !defined match('');
ok !defined match();

##

*match = re::DFA::Perl->as_method('a|b|c');

is match('a'), 'a';
is match('b'), 'b';
is match('c'), 'c';
is match('abc'), 'a';
is match('bac'), 'b';
is match('cde'), 'c';
ok !defined match('');
ok !defined match('d');
ok !defined match(' a');

##

*match = re::DFA::Perl->as_method('(())*');

is match('a'), '';
is match(''), '';

##

*match = re::DFA::Perl->as_method('\(|\)|\*|\|');

is match('(b'), '(';
is match(')'), ')';
is match('*'), '*';
is match('|'), '|';
ok !defined match('a');
