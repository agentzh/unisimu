# LL1_parser.t

use Test::Base;

use LL1_Parser;
use t::Util qw/ parse_grammar /;

plan tests => 1 * blocks();

run {
    my $block = shift;
    my $name = $block->name;
    my $grammar = $block->grammar;
    my $s_grammar = $block->s_grammar;
    my $expect_ast = parse_grammar($s_grammar);
    my $parser = LL1::Parser->new;
    my $ast = $parser->parse($grammar);
    is_deeply($ast, $expect_ast, "$name - AST comparison");
};

__DATA__

=== TEST 1: basic
--- grammar

foo: bar baz

--- s_grammar

foo: bar baz



=== TEST 2: multiple choices
--- grammar

foo: bar
   | baz

--- s_grammar

foo: bar | baz



=== TEST 3: multiple choices (with terminals)
--- grammar

foo: /.../ bar
   | baz 'foo'

--- s_grammar

foo: /.../ bar | baz 'foo'



=== TEST 4: empty production
--- grammar

foo:
   | 'abc'

--- s_grammar

foo: | 'abc'



=== TEST 5: empty production (tailing)
--- grammar

foo: 'abc'
   |

--- s_grammar

foo: 'abc' |



=== TEST 6: strings of balanced parentheses
--- grammar

S : '(' S ')' S | ''

--- s_grammar

S: '(' S ')' S | ''



=== TEST 7: if_statements
--- grammar

statement : if_stmt
          | 'other'

if_stmt   :'if' '(' exp ')' statement else_part

else_part : 'else'
            statement
          |

exp       : '0' | '1'

--- s_grammar

statement: if_stmt | 'other'
if_stmt: 'if' '(' exp ')' statement else_part
else_part: 'else' statement |
exp: '0' | '1'



=== TEST 8: simple integer expression (w/o left recursion)
--- grammar

exp
: 
term
exp_

exp_  :
addop term exp_ 
                 | ''
addop: '+' 
| '-' term: factor term_
term_: mulop factor term_ | ''
mulop: '*'
factor: '(' exp ')' | /\d+/

--- s_grammar

exp: term exp_
exp_: addop term exp_ | ''
addop: '+' | '-'
term: factor term_
term_: mulop factor term_ | ''
mulop: '*'
factor: '(' exp ')' | /\d+/



=== TEST 9: statement sequences
--- grammar

stmt_sequence: stmt stmt_seq stmt_seq: ';' stmt_sequence | stmt: 's' 

--- s_grammar

stmt_sequence: stmt stmt_seq
stmt_seq: ';' stmt_sequence |
stmt: 's'

