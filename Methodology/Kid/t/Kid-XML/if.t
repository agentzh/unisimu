#: if.t

use strict;
use warnings;

use t::Kid_XML;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
if (x > 5) { x:=x+1 }
--- xml
<?xml version="1.0"?>
<program>
<statement_list>
<statement>
<if_statement>
<condition>
<expression>
<term>
<factor>
<var>
<identifier>x</identifier>
</var>
</factor>
</term>
</expression>
<rel_op>&gt;</rel_op>
<expression>
<term>
<factor>
<number>5</number>
</factor>
</term>
</expression>
</condition>
<block>
<statement_list>
<statement>
<assignment>
<var>
<identifier>x</identifier>
</var>
<expression>
<term>
<factor>
<var>
<identifier>x</identifier>
</var>
</factor>
</term>
<op>+</op>
<term>
<factor>
<number>1</number>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</block>
</if_statement>
</statement>
</statement_list>
</program>



=== TEST 2
--- kid
if (6.3<= 0.232) {
    x:=2;
}
if (5 = x_) { yylex:=1 }
--- xml
<?xml version="1.0"?>
<program>
<statement_list>
<statement>
<if_statement>
<condition>
<expression>
<term>
<factor>
<number>6.3</number>
</factor>
</term>
</expression>
<rel_op>&lt;=</rel_op>
<expression>
<term>
<factor>
<number>0.232</number>
</factor>
</term>
</expression>
</condition>
<block>
<statement_list>
<statement>
<assignment>
<var>
<identifier>x</identifier>
</var>
<expression>
<term>
<factor>
<number>2</number>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</block>
</if_statement>
</statement>
<statement>
<if_statement>
<condition>
<expression>
<term>
<factor>
<number>5</number>
</factor>
</term>
</expression>
<rel_op>=</rel_op>
<expression>
<term>
<factor>
<var>
<identifier>x_</identifier>
</var>
</factor>
</term>
</expression>
</condition>
<block>
<statement_list>
<statement>
<assignment>
<var>
<identifier>yylex</identifier>
</var>
<expression>
<term>
<factor>
<number>1</number>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</block>
</if_statement>
</statement>
</statement_list>
</program>



=== TEST 3
--- kid
if (5>x) { x:= 3; } else {
    y:=-1 }
--- xml
<?xml version="1.0"?>
<program>
<statement_list>
<statement>
<if_statement>
<condition>
<expression>
<term>
<factor>
<number>5</number>
</factor>
</term>
</expression>
<rel_op>&gt;</rel_op>
<expression>
<term>
<factor>
<var>
<identifier>x</identifier>
</var>
</factor>
</term>
</expression>
</condition>
<block>
<statement_list>
<statement>
<assignment>
<var>
<identifier>x</identifier>
</var>
<expression>
<term>
<factor>
<number>3</number>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</block>
<block>
<statement_list>
<statement>
<assignment>
<var>
<identifier>y</identifier>
</var>
<expression>
<term>
<factor>
<number>-1</number>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</block>
</if_statement>
</statement>
</statement_list>
</program>
--- LAST



=== TEST 4
--- kid
if (x > 0) {
    x := 1;
    if (x < y) {
        y := 2;
    }
} else {
    if (y < x) {
        y := 3
    } else {
        y := 4
    }
    x := 5;
}
--- xml
