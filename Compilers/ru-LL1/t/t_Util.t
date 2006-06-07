# t_Util.t
# Test t::Util

use strict;
use warnings;

use Test::More tests => 5;
use Set::Scalar;
use t::Util qw( parse_grammar dump_fsets dump_LL1_table );

##
#  Test parse_grammar

#
# Test 1
#

my $expect_ast = {
    startrule => 'exp',
    rules => {
        exp    => [ ['exp', 'addop', 'term'], ['term'] ],
        addop  => [ ["'+'"], ["'-'"]],
        term   => [ ['term', 'mulop', 'factor'], ['factor'] ],
        mulop  => [ ["'*'"] ],
        factor => [ ["'('", 'exp', "')'"], ["'number'"] ],
    },
};

my $ast = parse_grammar(<<EOC);

 exp : exp addop term | term

 addop: '+' | '-'
 term :term mulop  factor  |factor

 mulop: '*'
 factor: '(' exp ')' | 'number'

EOC

is_deeply($ast, $expect_ast, 'AST ok');

#
# Test 2
#

$expect_ast = {
    startrule => 'A',
    rules => {
        A => [ [] ],
        B => [ ["''"] ],
        C => [ ["'abc'"], [] ],
        D => [ ["'D'"], ["''"] ],
    },
};

$ast = parse_grammar(<<EOC);
A:
B: ''
C: 'abc' |
D: 'D' | ''
EOC

is_deeply($ast, $expect_ast, 'AST with nulls ok');

##
#  Test dump_fsets

sub set {
    Set::Scalar->new(@_);
}

my $expect = <<EOC;
addop: '+' '-'
exp: '(' 'number'
factor: '(' 'number'
mulop: '*'
term: '(' 'number'
EOC

my $got = dump_fsets(
    {
        addop  => set(qw/ '+' '-'/),
        exp    => set(qw/ '(' 'number' /),
        factor => set(qw/ '(' 'number' /),
        mulop  => set(qw/ '*' /),
        term   => set(qw/ '(' 'number' /),
    }
);

is($got, $expect, 'Dump string ok');

##

$expect = <<'EOC';
B: $ '2' '3'
EOC

$got = dump_fsets(
    { 
        B => set(LL1::eof, qw[ '2' '3' ])
    }
);
is $got, $expect, 'Dump $ ok';

#
# Test sub dump_LL1_table
#

my $table = {
    'S' => {
        q/'('/ => [ qw/'(' S ')' S/ ],
        q/')'/ => [],
        LL1::eof => [ qw/''/ ],
    },
    'exp' => {
        q/'a'/ => [ 'E' ],
    },
};

$got = dump_LL1_table($table);

$expect = <<'_EOC_';
S
  $: S -> ''
  '(': S -> '(' S ')' S
  ')': S ->
exp
  'a': exp -> E
_EOC_

is $got, $expect, 'dump_LL1_table ok';
