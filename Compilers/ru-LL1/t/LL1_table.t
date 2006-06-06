#: LL1_table.t
#: Test LL1::Table

use strict;
use warnings;

use Set::Scalar;
use Test::More 'no_plan';

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
