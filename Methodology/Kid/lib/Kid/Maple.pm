#: Kid/Maple.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-22 2006-04-22

package Kid::Maple;

use strict;
use warnings;

#use Data::Dumper;
use Kid;
use Language::AttributeGrammar;

sub translate {
    my $src = shift;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ast = $parser->program($src) or return undef;
    return emit_maple($ast);
}

sub emit_maple {
    my $ast = shift;
    my $grammar = new Language::AttributeGrammar <<'END_GRAMMAR';
number:     $/.maple = { $<__VALUE__> }
factor:     $/.maple = { ::emit_factor($<child>.maple) }

term:       $/.maple = { $<term>.maple . $<op> . $<factor>.maple }
expression: $/.maple = { $<expression>.maple . $<op> . $<term>.maple }

nil:        $/.maple = { '' }
identifier: $/.maple = { $<__VALUE__> }
var:        $/.maple = { $<identifier>.maple }

assignment: $/.maple = { $<var>.maple . ':=' . $<expression>.maple . ";\n" }

block:           $/.maple = { "do\n" . $<statement_list>.maple . "od;\n" }
else_block:      $/.maple = { $<block>.maple }
rhs_expression:  $/.maple = { $<expression>.maple }

rel_op:       $/.maple = { $<__VALUE__> }
condition:    $/.maple = { $<expression>.maple . $<rel_op>.maple . $<rhs_expression>.maple }
if_statement: $/.maple = { ::emit_if( $<condition>.maple, $<block>.maple, $<else_block>.maple ); }

statement:      $/.maple = { $<child>.maple }
statement_list: $/.maple = { $<statement_list>.maple . $<statement>.maple }

program:    $/.maple = { $<statement_list>.maple }

END_GRAMMAR
    return $grammar->apply($ast, 'maple');
}

package main;

use strict;
use warnings;
use Scalar::Util qw( looks_like_number );

sub emit_factor {
    my $s = shift;
    return (looks_like_number($s) || $s =~ /^\(.*\)$/ || $s =~ /^\w+$/) ? $s : "($s)";
}

sub emit_if {
    my ($cond, $if_block, $else_block) = @_;
    if ($else_block) {
        return "if $cond then ${if_block}else ${else_block}fi;";
    } else {
        return "if $cond then ${if_block}fi;";
    }
}

1;
