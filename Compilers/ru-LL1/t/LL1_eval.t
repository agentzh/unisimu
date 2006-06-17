# LL1_eval.t
# Test LL1::Eval

use Test::Base;

use LL1_parser;
use LL1_eval;

plan tests => 2 * blocks() + 14;

my $Ast;

#$::LL1_TRACE = 1;
#$::LL1::Table::Trace = 1;
$::LL1_QUIET = 1;

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
    #warn "BBB @$X::tokens";
    my $match = LL1::Eval->eval($Ast, $input);
    #warn "!!! @$X::tokens";
    if (defined $block->error) {
        ok !defined, "$name - input parsing failed as expected";
        is( LL1::Eval->error, $block->error,
            "$name - input parsing diagnostics" );
        is( LL1::Eval->offset(), $block->offset,
            "$name - input parsing offset" );
    } else {
        ok defined $match, "$name - input parsing okay";
        if (!defined $match and !defined $block->error) {
            warn "$name - input parsing error:\n  ",
                LL1::Eval->error, "\n";
        }
        is $X::pos, length($input), "$name - input buffer consumed";
        if (defined $block->match) {
            is $match, $block->match, "$name - match object okay";
        }
    }
};

__DATA__

=== TEST 1: basic
--- grammar

# This is an identifier
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

S: '(' S ')' S | '' # empty token

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



=== TEST 9: if statement
--- grammar

  # rule for statement:

statement: if_stmt
         | /\w+/  # other

 # rule for if_stmt:

if_stmt  : 'if' '(' exp ')' statement else_part

 # rule for else_part:

else_part: 'else' statement
         |

 # rule for exp:

exp      : '0' | "1"

--- input
if (0) other



=== TEST 10:
--- input
if (0) if (1) cry else laugh



=== TEST 11: if statement grammar with bogus production order
--- grammar

statement: /\w+/
         | if_stmt

if_stmt  : 'if' '(' exp ')' statement else_part

else_part: "else" statement
         |

exp      : '0' | '1'

--- input
if (0) other



=== TEST 12: if statement grammar with bogus production order
--- grammar

statement: /\w+/
         | if_stmt

if_stmt  : 'if' '(' exp ')' statement else_part

else_part: "else" statement
         |

exp      : '0' | '1'

<token: 'if' '(' ')' "else" '0' '1' /\w+/>

--- input
if (0) other



=== TEST 13: Logical expressions
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



=== TEST 14:
--- input
T



=== TEST 15:
--- input
F AND (T OR F
--- error
Was expecting ')', but found EOF instead
--- offset
13



=== TEST 16:
--- input
NOT 32
--- error
Was expecting brack_expr, but found '32' instead
--- offset
3



=== TEST 17:
--- input
NOT F



=== TEST 18: equivalent regex conflicts
--- grammar

float: /\d+/ '.' /[0-9]+/

--- input
3.14



=== TEST 19: equivalent regex conflicts (II)
--- grammar

assignment: var ':=' exp

var: /[A-Za-z]\w*/

exp: /\d+/
   | /[a-zA-Z]\w*/

--- input
a := b



=== TEST 20: strings of common prefix
--- grammar

stmt: 'if'
    | var

var: /[A-Za-z]\w*/

--- input
if_stmt



=== TEST 21: lexer bug
--- grammar

if_stmt: var ':=' expr 'if' expr ';'
var: /[A-Za-z]\w*/
expr: /\d+/

--- input
foo := 32 if 1;
