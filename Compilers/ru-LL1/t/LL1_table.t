#: LL1_table.t
#: Test LL1::Table

use strict;
use warnings;

use Set::Scalar;
use Test::More tests => 42;

BEGIN { use_ok('LL1_table'); }

# Test set_insert:

*set_insert = \&LL1::Table::set_insert;

sub fmt {
    my $set = shift;
    join ' ', (sort $set->elements);
}

sub set {
    Set::Scalar->new(@_);
}

my $A = set(1,2,3);

is set_insert($A, 1), 0;
is fmt($A), '1 2 3';

is set_insert($A, 2), 0;
is fmt($A), '1 2 3';

is set_insert($A, 3), 0;
is fmt($A), '1 2 3';

is set_insert($A, 4), 1;
is fmt($A), '1 2 3 4';

is set_insert($A, 'a'), 1;
is fmt($A), '1 2 3 4 a';

is set_insert($A, 4), 0;
is fmt($A), '1 2 3 4 a';

is set_insert($A, 'a'), 0;
is fmt($A), '1 2 3 4 a';

# Test set_add:

* set_add = \&LL1::Table::set_add;

$A = set(1,2,4);

is set_add($A, set(1,4,5)), 1;
is fmt($A), '1 2 4 5';

is set_add($A, set(2,5)), 0;
is fmt($A), '1 2 4 5';

is set_add($A, set(7,8)), 2;
is fmt($A), '1 2 4 5 7 8';

is set_add($A, $A->copy), 0;
is fmt($A), '1 2 4 5 7 8';

my $ast = {
    startrule => 'exp',
    rules => {
        exp    => [ ['exp', 'addop', 'term'], ['term'] ],
        addop  => [ ["'+'"], ["'-'"]],
        term   => [ ['term', 'mulop', 'factor'], ['factor'] ],
        mulop  => [ ["'*'"] ],
        factor => [ ["'('", 'exp', "')'"], ["'number'"] ],
    },
};

*first_sets = \&LL1::Table::first_sets;

my $Firsts = first_sets($ast);
is join(' ', sort keys %$Firsts), 'addop exp factor mulop term';

my $first = $Firsts->{exp};
is fmt($first), "'(' 'number'", 'First(exp)';

$first = $Firsts->{term};
is fmt($first), "'(' 'number'", 'First(term)';

$first = $Firsts->{factor};
is fmt($first), "'(' 'number'", 'First(factor)';

$first = $Firsts->{addop};
is fmt($first), "'+' '-'", 'First(addop)';

$first = $Firsts->{mulop};
is fmt($first), "'*'", 'First(mulop)';

#
# Test sub string_first_set
#

*string_first_set = \&LL1::Table::string_first_set;

$Firsts = {
    A => set(qw/ '' 'a' /),
    B => set(qw/ '' 'b' /),
    C => set(qw/ '' 'c' 'd' /),
};
my $set = string_first_set($Firsts, qw/A B C/);
is fmt($set), q('' 'a' 'b' 'c' 'd'), 'all 3 nonterminals nullable';

$Firsts = {
    A => set(qw/'' 'a'/),
    B => set(qw/'' 'b'/),
    C => set(qw/'c' 'd'/),
};
$set = string_first_set($Firsts, qw/A B C/);
is fmt($set), q('a' 'b' 'c' 'd'), 'only the first 2 of 3 nonterminals nullable';

$Firsts = {
    A => set(qw/'' 'a'/),
    B => set(qw/'b'/),
    C => set(qw/'c' 'd'/),
};
$set = string_first_set($Firsts, qw/A B C/);
is fmt($set), q('a' 'b'), 'only the first 1 of 3 nonterminals nullable';

$Firsts = {
    A => set(qw/'a'/),
    B => set(qw/'' 'b'/),
    C => set(qw/'c' 'd' ''/),
};
$set = string_first_set($Firsts, qw/A B C/);
is fmt($set), q/'a'/, 'only the first 1 of 3 nonterminals not nullable';

$set = string_first_set($Firsts, qw/'a'/);
is fmt($set), q/'a'/, 'only 1 string (terminal)';

$set = string_first_set($Firsts, '/\w+/');
is fmt($set), '/\w+/', 'only 1 regex (terminal)';

$set = string_first_set($Firsts, qw('a' A));
is fmt($set), q/'a'/, '1 terminal and 1 nonterminal';

$set = string_first_set($Firsts, qw('a' /\w+/));
is fmt($set), q/'a'/, '2 terminals';

$set = string_first_set($Firsts, qw(B C /\w+/));
is fmt($set), q('b' 'c' 'd' /\w+/), 'continues to the terminal';

$set = string_first_set($Firsts, qw(B C /\w+/ A));
is fmt($set), q('b' 'c' 'd' /\w+/),
    '1 terminal blocks the continuation to another nonterminal';

$set = string_first_set($Firsts, qw(B C 'h' /\d+/));
is fmt($set), q('b' 'c' 'd' 'h'),
    '1 terminal blocks the continuation to another terminal';

$set = string_first_set($Firsts, qw(''));
is fmt($set), q(''), "Follow(eps) = { eps }";

$set = string_first_set($Firsts);
is fmt($set), q(''), "Follow() = { eps }";
