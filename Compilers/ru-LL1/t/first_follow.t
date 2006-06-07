# first_follow.t
# Test First/Follow sets generators

use Test::Base;
use t::Util qw/ parse_grammar dump_fsets /;
use LL1_table;

plan tests => 2 * blocks();

#$LL1::Table::Trace = 1;

run {
    my $block = shift;
    my $name = $block->name;
    my $ast = parse_grammar($block->grammar);
    my $fi_sets = LL1::Table::first_sets($ast);
    is dump_fsets($fi_sets), $block->first_sets, "$name - First sets";
    my $fo_sets = LL1::Table::follow_sets($ast, $fi_sets);
    is dump_fsets($fo_sets), $block->follow_sets, "$name - Follow sets";
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

--- follow_sets
A: $
B: $ '2' '3'
C: $ '3'
D: $



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

--- follow_sets
A: $
B: '2' '3'
C: '3'
D: $



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

--- follow_sets
A: $
B: '2' '3'
C: '3'
D: $



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

--- follow_sets
A: $
B: '2'
C: '3'
D: $



=== TEST 5: no item is nullable
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

--- follow_sets
A: $
B: '2'
C: '3'
D: $



=== TEST 6: simple integer expression grammar
--- grammar

exp   : exp addop term | term
addop : '+' | '-'
term  : term mulop factor | factor
mulop : '*'
factor: '(' exp ')' | /\d+/

--- first_sets
addop: '+' '-'
exp: '(' /\d+/
factor: '(' /\d+/
mulop: '*'
term: '(' /\d+/

--- follow_sets
addop: '(' /\d+/
exp: $ ')' '+' '-'
factor: $ ')' '*' '+' '-'
mulop: '(' /\d+/
term: $ ')' '*' '+' '-'



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

--- follow_sets
else_part: $ 'else'
exp: ')'
if_stmt: $ 'else'
statement: $ 'else'



=== TEST 8: grammar for statement sequences
--- grammar

stmt_sequence : stmt stmt_seq
stmt_seq      : ';' stmt_sequence | ''
stmt          : 's'

--- first_sets
stmt: 's'
stmt_seq: '' ';'
stmt_sequence: 's'

--- follow_sets
stmt: $ ';'
stmt_seq: $
stmt_sequence: $



=== TEST 9: simple integer expression grammar w/o left recursion
--- grammar

exp: term exp_
exp_: addop term exp_ | ''
addop: '+' | '-'
term: factor term_
term_: mulop factor term_ | ''
mulop: '*'
factor: '(' exp ')' | /\d+/

--- first_sets
addop: '+' '-'
exp: '(' /\d+/
exp_: '' '+' '-'
factor: '(' /\d+/
mulop: '*'
term: '(' /\d+/
term_: '' '*'

--- follow_sets
addop: '(' /\d+/
exp: $ ')'
exp_: $ ')'
factor: $ ')' '*' '+' '-'
mulop: '(' /\d+/
term: $ ')' '+' '-'
term_: $ ')' '+' '-'
