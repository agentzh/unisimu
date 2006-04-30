#: Kid/XML.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-23 2006-04-30

package Kid::XML;

use strict;
use warnings;

#use Data::Dumper;
use Kid;
use Language::AttributeGrammar;

our $VERSION = '0.01';

our $Grammar;

sub translate {
    my $src = shift;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ast = $parser->program($src) or return undef;
    return "<?xml version=\"1.0\"?>\n" . emit_xml($ast);
}

sub emit_xml {
    my $ast = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

expression_list: $/.xml = { Kid::XML::emit_expr_list( $<expression_list>.xml, $<expression>.xml ); }
proc_call:  $/.xml = { Kid::XML::emit_proc_call( $<identifier>.xml, $<expression_list>.xml ); }

number:     $/.xml = { "<number>" . $<__VALUE__> . "</number>\n" }
factor:     $/.xml = { Kid::XML::emit_factor( $<child>.xml ); }

neg:        $/.xml = { "<negative/>\n" }
term:       $/.xml = { $<neg>.xml . $<term>.xml . Kid::XML::emit_op( $<op> ) . $<factor>.xml }
expression: $/.xml = { Kid::XML::emit_expr( $<expression>.xml, $<op>, $<term>.xml ) }

nil:        $/.xml = { '' }
identifier: $/.xml = { "<identifier>" . $<__VALUE__> . "</identifier>\n" }
var:        $/.xml = { "<var>\n" . $<identifier>.xml . "</var>\n" }

assignment: $/.xml = { Kid::XML::emit_assign( $<var>.xml, $<expression>.xml ) }

block:           $/.xml = { Kid::XML::emit_block( $<statement_list>.xml ); }
else_statement:  $/.xml = { $<statement>.xml }
rhs_expression:  $/.xml = { $<expression>.xml }

rel_op:       $/.xml = { "<rel_op>" . Kid::XML::escape( $<__VALUE__> ) . "</rel_op>\n" }
condition:    $/.xml = { Kid::XML::emit_cond( $<expression>.xml, $<rel_op>.xml, $<rhs_expression>.xml ) }
if_statement: $/.xml = { Kid::XML::emit_if( $<condition>.xml, $<statement>.xml, $<else_statement>.xml ) }

comment: $/.xml = { "<comment>" . $<__VALUE__> . "</comment>\n"; }

identifier_list: $/.xml = { $<identifier_list>.xml . $<identifier>.xml }
proc_decl:   $/.xml = { Kid::XML::emit_proc_decl( $<identifier>.xml, $<identifier_list>.xml, $<block>.xml ) }

declaration: $/.xml = { "<declaration>\n" . $<child>.xml . "</declaration>\n"; }

statement:      $/.xml = { "<statement>\n" .$<child>.xml . "</statement>\n" }
statement_list: $/.xml = { $<statement_list>.xml . $<statement>.xml }

program:    $/.xml = { Kid::XML::emit_program( $<statement_list>.xml ); }

END_GRAMMAR
    return $Grammar->apply($ast, 'xml');
}

sub emit_program {
    my ($statement_list) = @_;
    return <<EOC;
<program>
<statement_list>
${statement_list}</statement_list>
</program>
EOC
}

sub emit_cond {
    my ($lhs, $op, $rhs) = @_;
    return <<EOC;
<condition>
<expression>
$lhs</expression>
$op<expression>
$rhs</expression>
</condition>
EOC
}

sub emit_if {
    my ($cond, $then, $else) = @_;
    return <<EOC;
<if_statement>
${cond}${then}${else}</if_statement>
EOC
}

sub emit_assign {
    my ($var, $expr) = @_;
    return <<EOC;
<assignment>
$var<expression>
$expr</expression>
</assignment>
EOC
}

sub emit_block {
    my ($stmt_list) = @_;
    return <<EOC;
<block>
<statement_list>
$stmt_list</statement_list>
</block>
EOC
}

sub emit_op {
    my $op = shift;
    return '' if !defined $op;
    $op = escape($op);
    "<op>$op</op>\n";
}

sub emit_expr {
    my ($expr, $op, $term) = @_;
    $op = emit_op($op);
    return <<EOC;
${expr}${op}<term>
$term</term>
EOC
}

sub emit_factor {
    my ($child) = @_;
    if ($child =~ /^<term>/) {
        return <<EOC;
<factor>
<expression>
$child</expression>
</factor>
EOC
    }
    "<factor>\n$child</factor>\n";
}

sub emit_proc_decl {
    my ($proc_name, $param_list, $block) = @_;
    "<proc_decl>\n$proc_name<identifier_list>\n$param_list</identifier_list>\n$block</proc_decl>\n";
}

sub emit_proc_call {
    my ($proc_name, $arg_list) = @_;
    "<proc_call>\n$proc_name<expression_list>\n$arg_list</expression_list>\n</proc_call>\n";
}

sub emit_expr_list {
    my ($expr_list, $expr) = @_;
    "$expr_list<expression>\n$expr</expression>\n"
}

sub escape {
    my $s = shift;
    #warn "++ ESCAPE: $s";
    $s =~ s/\&/\&amp;/g;
    $s =~ s/</\&lt;/g;
    $s =~ s/>/\&gt;/g;
    #warn "-- ESCAPE: $s";
    return $s;
}

1;
