#: Kid/Maple.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-22 2006-04-24

package Kid::Maple;

use strict;
use warnings;

#use Data::Dumper;
use Kid;
use Language::AttributeGrammar;
use Scalar::Util qw( looks_like_number );

our $Grammar;

sub translate {
    my $src = shift;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ast = $parser->program($src) or return undef;
    return emit_maple($ast);
}

sub emit_maple {
    my $ast = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

number:     $/.maple = { $<__VALUE__> }
factor:     $/.maple = { Kid::Maple::emit_factor($<child>.maple) }

term:       $/.maple = { $<term>.maple . $<op> . $<factor>.maple }
expression: $/.maple = { $<expression>.maple . $<op> . $<term>.maple }

nil:        $/.maple = { '' }
identifier: $/.maple = { $<__VALUE__> }
var:        $/.maple = { $<identifier>.maple }

assignment: $/.maple = { $<var>.maple . ':=' . $<expression>.maple . ";\n" }

block:           $/.maple = { $<statement_list>.maple }
else_block:      $/.maple = { $<block>.maple }
rhs_expression:  $/.maple = { $<expression>.maple }

rel_op:       $/.maple = { $<__VALUE__> }
condition:    $/.maple = { $<expression>.maple . $<rel_op>.maple . $<rhs_expression>.maple }
if_statement: $/.maple = { Kid::Maple::emit_if( $<condition>.maple, $<block>.maple, $<else_block>.maple ); }

statement:      $/.maple = { $<child>.maple }
statement_list: $/.maple = { $<statement_list>.maple . $<statement>.maple }

program:    $/.maple = { $<statement_list>.maple }

END_GRAMMAR
    return $Grammar->apply($ast, 'maple');
}

sub emit_factor {
    my $s = shift;
    return (looks_like_number($s) || $s =~ /^\(.*\)$/ || $s =~ /^\w+$/) ? $s : "($s)";
}

sub emit_if {
    my ($cond, $then, $else) = @_;
    if ($else) {
        return "if $cond then\n${then}else\n${else}end if;\n";
    } else {
        return "if $cond then\n${then}end if;\n";
    }
}

1;
