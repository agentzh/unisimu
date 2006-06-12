# LL1_eval
# Test LL1::Eval

use Test::Base;

use LL1_parser;
use LL1_eval;

plan tests => 2 * blocks() + 8;

my $Ast;

$LL1::Runtime::Trace = 1;

filters {
    offset => ['chomp'],
    error  => ['chomp'],
};

run {
    my $block = shift;
    my $name = $block->name;
    my $parser = LL1::Parser->new;
    if ($block->grammar) {
        $Ast = $parser->parse($block->grammar);
        ok defined $Ast, "$name - grammar parsing okay";
    }
    my $input = $block->input;
    my $match = LL1::Eval->eval($Ast, $input);
    #warn "!!! @$X::tokens";
    if (defined $block->error) {
        ok !defined, "$name - input parsing failed as expected";
        is( LL1::Eval->error, $block->error,
            "$name - input parsing diagnostics" );
        is( LL1::Eval->offset(), $block->offset,
            "$name - input parsing offset" );
    } else {
        ok $match, "$name - input parsing okay";
        is $X::pos, length($input), "$name - input buffer consumed";
        if (defined $block->match) {
            is $match, $block->match, "$name - match object okay";
        }
    }
};

__DATA__

=== TEST 1: basic
--- grammar

identifier: /[A-Za-z]\w*/

--- input
 foo32



=== TEST 2:
--- input
  12
--- error
Was expecting identifier, but found '12' instead
--- offset
0



=== TEST 3:
--- grammar

S: '(' S ')' S | ''

--- input
()



=== TEST 4:
--- input
 (  )



=== TEST 5:
--- input
(
--- error
Was expecting ')', but found EOF instead
--- offset
1



=== TEST 6:
--- input
() ( )



=== TEST 7:
--- input
(( ) )



=== TEST 8:
--- input
(()())()



=== TEST 9:
--- grammar

statement: if_stmt
         | /\S+/

if_stmt  : 'if' '(' exp ')' statement else_part

else_part: 'else' statement
         |

exp      : '0' | '1'

--- input
if (0) other



=== TEST 10: Logical expressions
--- grammar

root: expr

expr: and_expr expr_

expr_: 'OR' expr
     |

and_expr:   not_expr and_expr_

and_expr_: 'AND' and_expr
         |

not_expr: 'NOT' brack_expr
        | brack_expr

brack_expr: '(' expr ')'
          | atom

atom: 'T'
    | 'F'

--- input
F OR (T AND (F OR F)) OR F



=== TEST 11:
--- input
T



=== TEST 12:
--- input
F AND (T OR F
--- error
Was expecting ')', but found EOF instead
--- offset
13



=== TEST 13:
--- input
NOT 32
--- error
Was expecting brack_expr, but found '32' instead
--- offset
3



=== TEST 14:
--- input
NOT F
