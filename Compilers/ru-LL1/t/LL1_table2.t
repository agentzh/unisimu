# LL1_table2.t
# Test the LL1_table sub

use Test::Base;

use LL1_table;
use t::Util qw/ parse_grammar dump_LL1_table /;

plan tests => 1 * blocks();

$::LL1_QUIET = 1;

run {
    my $block = shift;
    my $name = $block->name;
    my $ast = parse_grammar($block->grammar);
    my $Firsts = LL1::Table::first_sets($ast);
    my $Follows = LL1::Table::follow_sets($ast, $Firsts);
    my $table = LL1::Table::LL1_table($ast, $Firsts, $Follows);
    my $got = dump_LL1_table($table);
    is $got, $block->LL1_table, $name;
};

__DATA__

=== TEST 1: strings of balanced parentheses
--- grammar

S: '(' S ')' S | ''

--- LL1_table
S
  $: S ->
  '(': S -> '(' S ')' S
  ')': S ->



=== TEST 2: if_statements
--- grammar

statement: if_stmt | 'other'
if_stmt: 'if' '(' exp ')' statement else_part
else_part: 'else' statement |
exp: '0' | '1'

--- LL1_table
else_part
  $: else_part ->
  'else': else_part -> 'else' statement
exp
  '0': exp -> '0'
  '1': exp -> '1'
if_stmt
  'if': if_stmt -> 'if' '(' exp ')' statement else_part
statement
  'if': statement -> if_stmt
  'other': statement -> 'other'



=== TEST 3: simple integer expression (w/o left recursion)
--- grammar

exp: term exp_
exp_: addop term exp_ | ''
addop: '+' | '-'
term: factor term_
term_: mulop factor term_ | ''
mulop: '*'
factor: '(' exp ')' | /\d+/

--- LL1_table
addop
  '+': addop -> '+'
  '-': addop -> '-'
exp
  '(': exp -> term exp_
  /\d+/: exp -> term exp_
exp_
  $: exp_ ->
  ')': exp_ ->
  '+': exp_ -> addop term exp_
  '-': exp_ -> addop term exp_
factor
  '(': factor -> '(' exp ')'
  /\d+/: factor -> /\d+/
mulop
  '*': mulop -> '*'
term
  '(': term -> factor term_
  /\d+/: term -> factor term_
term_
  $: term_ ->
  ')': term_ ->
  '*': term_ -> mulop factor term_
  '+': term_ ->
  '-': term_ ->



=== TEST 4: statement sequences
--- grammar

stmt_sequence: stmt stmt_seq
stmt_seq: ';' stmt_sequence |
stmt: 's'

--- LL1_table
stmt
  's': stmt -> 's'
stmt_seq
  $: stmt_seq ->
  ';': stmt_seq -> ';' stmt_sequence
stmt_sequence
  's': stmt_sequence -> stmt stmt_seq
