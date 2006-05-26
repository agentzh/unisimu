# action.t
# Test user-defined actions in grammars

use strict;
use warnings;

use test_spike;
use Text::Balanced;

plan tests => 1 * blocks() + 3 * 8;

run_tests;

__DATA__

=== TEST 1: basic
--- grammar

if_stmt: 'if' cond block { [@item] }

cond: '(' /\d+/ ')' { [@item] }

block: '{' /[^}]*/ '}' { [@item] }

--- input
if (32) {
    say "hello";
}
--- ast
['if_stmt', 'if', ['cond', '(', 32, ')'], ['block', '{', qq{say "hello";\n}, '}']]



=== TEST 2: <commit> and <uncommit> are also in @item
--- grammar

if_stmt: 'if' <commit> cond block <uncommit> 'else' block { [@item] }

cond: '(' /\d+/ ')' { [@item] }

block: '{' /[^}]*/ '}' { [@item] }

--- input
if (32) {a} else {b}
--- ast
[
 'if_stmt', 'if', '<commit>', ['cond', '(', 32, ')'], ['block', '{', 'a', '}'],
 '<uncommit>', 'else', ['block', '{', 'b', '}']
]



=== TEST 3: action is also a subrule per se ($text ok)
--- grammar

regex: {Text::Balanced::extract_delimited($text,'/')}

--- input
/\/(\\\/|[^\/])*\//
--- ast chop
<<'EOC';
/\/(\\\/|[^\/])*\//
EOC



=== TEST 4: action is also a subrule per se ($text and @item ok)
--- grammar

regex: {Text::Balanced::extract_delimited($text,'/')} {[@item]}

--- input
/\/(\\\/|[^\/])*\//
--- ast
$a = <<'EOC';
/\/(\\\/|[^\/])*\//
EOC
CORE::chop $a;
['regex',$a]



=== TEST 5: arithmetic calculator, single number
--- grammar

input: expr eof { $item[1] }

eof: /^\Z/

expr: <leftop: term /([-+])/ term>

        { eval join(' ', @{$item[1]}); }

term: <leftop: factor /([*\/])/ factor>

        { eval join(' ', @{$item[1]}); }

factor: neg(?) number

            { my $neg = $item[1]->[0] || ''; $neg . $item[2] }

      | '(' expr ')'  { $item[2] }

neg: '-'

number: /[1-9]\d*/

--- input
23
--- ast
23



=== TEST 6: arithmetic calculator, single negative
--- input
-1
--- ast
-1



=== TEST 7: arithmetic calculator, single term
--- input
3*15
--- ast
45



=== TEST 8: arithmetic calculator, complex
--- input
3-(5-6)
--- ast
4



=== TEST 9: arithmetic calculator, complex
--- input
3-(5+6)+5*(-20)/(2-1)
--- ast
-108



=== TEST 10: logical expression, right recursion
--- grammar

root: expression eof { $item[1] ? 'T' : 'F' }

eof: /^\Z/

expression: and_expr 'OR' expression
                { $item[1] || $item[3] }
          | and_expr

and_expr:   not_expr 'AND' and_expr
                { $item[1] && $item[3] }
        |   not_expr

not_expr:   'NOT' brack_expr
                { ! $item[2] }
        |   brack_expr

brack_expr: '(' expression ')'
                { $item[2] }
          | atom

atom: 'T' { 1 }
    | 'F' { 0 }

--- input
F OR (T AND (F OR F)) OR F
--- ast
'F'



=== TEST 11: ditto
--- input
F OR (T AND (F OR F)) OR T
--- ast
'T'



=== TEST 12: ditto
--- input
F OR (T AND (F OR F)) OR NOT F
--- ast
'T'



=== TEST 13: ditto
--- input
F OR (T AND (F OR T)) OR F
--- ast
'T'



=== TEST 14: logical expression, output prefix form
--- grammar

root: expression eof { $item[1] }

eof: /^\Z/

expression: and_expr 'OR' expression
                { "(or $item[1] $item[3])" }
          | and_expr

and_expr:   not_expr 'AND' and_expr
                { "(and $item[1] $item[3])" }
        |   not_expr

not_expr:   'NOT' brack_expr
                { "(not $item[2])" }
        |   brack_expr

brack_expr: '(' expression ')'
                { $item[2] }
          | atom

atom: 'T'
    | 'F'

--- input
F OR (T AND (F OR F)) OR NOT F
--- ast
'(or F (or (and T (or F F)) (not F)))'



=== TEST 15: cascaded actions work
--- grammar

string: {Text::Balanced::extract_delimited($text,'/')}
        {Text::Balanced::extract_codeblock($text)}

    { '*' . $item[1] . '*' . $item[2] . '*' }

--- input

/[A-Za-z]/ { say "hello!" }

--- ast
'*/[A-Za-z]/*{ say "hello!" }*'
