# first_follow.t
# Test First/Follow sets generators

use Test::Base;
use t::Util qw/ parse_grammar dump_fsets /;
use LL1_table;

plan tests => 1 * blocks();

#$LL1::Table::Trace = 1;

run {
    my $block = shift;
    my $name = $block->name;
    my $ast = parse_grammar($block->grammar);
    my $fi_sets = LL1::Table::first_sets($ast);
    is dump_fsets($fi_sets), $block->first_sets, "$name - First sets";
    #my $fo_sets = LL1::Table::follow_sets($ast);
    #is dump_fsets($fo_sets, $block->follow_sets, "$name - Follow sets";
};

__DATA__

=== TEST 1: all items nullable
--- grammar

A: B C D
B: '1' | ''
C: '2' | ''
D: '3' | ''

--- first_sets
A: '' '1' '2' '3'
B: '' '1'
C: '' '2'
D: '' '3'



=== TEST 2: Only the first 2 items nullable
--- grammar

A: B C D
B: '1' | ''
C: '2' | ''
D: '3'

--- first_sets
A: '1' '2' '3'
B: '' '1'
C: '' '2'
D: '3'



=== TEST 3: ditto (antoher form of grammar)
--- grammar

A: B C D
B: '1' |
C: '2' |
D: '3'

--- first_sets
A: '1' '2' '3'
B: '' '1'
C: '' '2'
D: '3'



=== TEST 4: Only the first item nullable
--- grammar

A: B C D
B: '1' | ''
C: '2'
D: '3'

--- first_sets
A: '1' '2'
B: '' '1'
C: '2'
D: '3'



=== TEST 5: on item is nullable
--- grammar

A: B C D
B: '1'
C: '2'
D: '3'

--- first_sets
A: '1'
B: '1'
C: '2'
D: '3'



=== TEST 6: simple integer expression grammar
--- grammar

exp   : exp addop term | term
addop : '+' | '-'
term  : term mulop factor | factor
mulop : '*'
factor: '(' exp ')' | 'number'

--- first_sets
addop: '+' '-'
exp: '(' 'number'
factor: '(' 'number'
mulop: '*'
term: '(' 'number'



=== TEST 7: grammar for if-statements
--- grammar

statement : if_stmt | 'other'
if_stmt   : 'if' '(' exp ')' statement else_part
else_part : 'else' statement | ''
exp       : '0' | '1'

--- first_sets
else_part: '' 'else'
exp: '0' '1'
if_stmt: 'if'
statement: 'if' 'other'



=== TEST 8: grammar for statement sequences
--- grammar

stmt_sequence : stmt stmt_seq
stmt_seq      : ';' stmt_sequence | ''
stmt          : 's'

--- first_sets
stmt: 's'
stmt_seq: '' ';'
stmt_sequence: 's'
