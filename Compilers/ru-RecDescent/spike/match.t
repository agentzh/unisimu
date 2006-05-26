# match.t

use strict;
use warnings;

use File::Temp qw/ tempfile /;
use Test::Base;

plan tests => 1 * blocks() + 3 * 5;

my $pmfile;
my @pmfiles;

mkdir 'tmp' if !-d 'tmp';

my $counter = 0;
my $Parser;

filters {
    ast => 'eval',
};

run {
    my $block = shift;
    my $gm = $block->grammar;
    my $input = $block->input;
    my $expected_ast = $block->ast;
    my $name = $block->name;

    if (defined $gm) {
        my ($fh, $gmfile) =
            tempfile('gm_XXXXXX', SUFFIX => '.grammar', UNLINK => 1, DIR => 'tmp');
        #warn "Grammar File: $gmfile";
        print $fh $gm;
        close $fh;
        my $class = 'Parser' . (++$counter);
        is system($^X, 'spike.pl', '-m', "-n $class", $gmfile), 0, "$name - spike.pl";
        ($pmfile = $gmfile) =~ s/\.grammar$/.pm/;
        ok -f $pmfile, "$name - $pmfile ok";
        ok require $pmfile, "$name - load module $pmfile ok";
        $Parser = $class->new;
    }

    my $ast = $Parser->parse($input);
    is_deeply $ast, $expected_ast, "$name - parse tree ok";
    push @pmfiles, $pmfile;
};

for my $file (@pmfiles) {
    unlink $pmfile;
}

__DATA__

=== TEST 1: match_re returns matched substring
--- grammar

identifier: /[A-Za-z]\w*/

--- input

match_re

--- ast
"match_re"



=== TEST 2: ditto, invalid input
--- input

232

--- ast
undef



=== TEST 3: match_str returns matched substring
--- grammar

keyword: 'procedure'
       | 'if'
       | 'while'

--- input
if
--- ast
'if'



=== TEST 4: match_str: 'while'
--- input
   while
--- ast
'while'



=== TEST 5: match_str: 'procedure'
--- input
procedure
--- ast
"procedure"



=== TEST 5: match_str, fail to match
--- input
for
--- ast
undef



=== TEST 6: concat returns last item by default
--- grammar

if_stmt: 'if' '(' /[01]/ ')' block

block: /{[^}]*}/

--- input
if (1) {
    say "ok!";
}
--- ast
q[{
    say "ok!";
}]



=== TEST 7: chained rules, if_statement
--- grammar
program: statement

statement: if_statement
         | assignment

if_statement: 'if' '(' expression ')' block

expression: /[1-9]\d+\b/

block: /{[^}]*}/

assignment: var ':=' expression

var: /[A-Za-z]\w*/

--- input
if (32) { print 'yay!' }

--- ast
"{ print 'yay!' }"



=== TEST 8: chained rules, assignment
--- input
foo := 25
--- ast
25



=== TEST 9: modifier '(s)', 3 elems
--- grammar
program: number(s)
       | ':' var(s?)
       | '=>' operator(?)

number: /[1-9]\d*/

var: /[A-Za-z]\w*/

operator: /[-+]/

--- input
1 2 3
--- ast
[1,2,3]



=== TEST 10: modifier '(s)', 1 elem
--- input
15
--- ast
[15]



=== TEST 11: modifier '(s?)', 3 elems
--- input
:foo bar baz
--- ast
[qw(foo bar baz)]



=== TEST 12: modifier '(s?)', 1 elem
--- input
:yay5
--- ast
['yay5']



=== TEST 13: modifier '(s?)', 0 elem
--- input
:
--- ast
[]



=== TEST 14: modifier '(?)', 1 elem
--- input
=> +
--- ast
['+']



=== TEST 14: modifier '(?)', 0 elem
--- input
=>
--- ast
[]
