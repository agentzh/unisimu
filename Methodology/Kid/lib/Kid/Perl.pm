#: Kid/Perl.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-22 2006-04-24

package Kid::Perl;

use strict;
use warnings;

#use Data::Dumper;
use Kid;
use Language::AttributeGrammar;

our $Grammar;

sub translate {
    my $src = shift;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ast = $parser->program($src) or return undef;
    return emit_perl($ast);
}

sub emit_perl {
    my $ast = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';
number:     $/.perl = { $<__VALUE__> }
factor:     $/.perl = { ::emit_factor($<child>.perl) }

term:       $/.perl = { $<term>.perl . $<op> . $<factor>.perl }
expression: $/.perl = { $<expression>.perl . $<op> . $<term>.perl }

nil:        $/.perl = { '' }
identifier: $/.perl = { $<__VALUE__> }
var:        $/.perl = { '$'.$<identifier>.perl }

assignment: $/.perl = { $<var>.perl . '=' . $<expression>.perl . ";\n" }

block:           $/.perl = { "{\n" . $<statement_list>.perl . "}\n" }
else_block:      $/.perl = { $<block>.perl }
rhs_expression:  $/.perl = { $<expression>.perl }

rel_op:       $/.perl = { ::emit_rel_op( $<__VALUE__> ) }
condition:    $/.perl = { $<expression>.perl . $<rel_op>.perl . $<rhs_expression>.perl }
if_statement: $/.perl = { ::emit_if( $<condition>.perl, $<block>.perl, $<else_block>.perl ); }

statement:      $/.perl = { $<child>.perl }
statement_list: $/.perl = { $<statement_list>.perl . $<statement>.perl }

program:    $/.perl = { $<statement_list>.perl }

END_GRAMMAR
    return $Grammar->apply($ast, 'perl');
}

package main;

use strict;
use warnings;
use Scalar::Util qw( looks_like_number );

sub emit_factor {
    my $s = shift;
    return (looks_like_number($s) || $s =~ /^\(.*\)$/ || $s =~ /^\$\w+$/) ? $s : "($s)";
}

sub emit_if {
    my ($cond, $if_block, $else_block) = @_;
    if ($else_block) {
        return "if($cond)${if_block}else$else_block";
    } else {
        return "if($cond)$if_block";
    }
}

sub emit_rel_op {
    my ($op) = @_;
    if ($op eq '=') { return '=='; }
    if ($op eq '<>') { return '!='; }
    $op;
}

1;
