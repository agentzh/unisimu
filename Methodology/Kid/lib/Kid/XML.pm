#: Kid/XML.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-23 2006-04-24

package Kid::XML;

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
    return "<?xml version=\"1.0\"?>\n" . emit_xml($ast);
}

sub emit_xml {
    my $ast = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

number:     $/.xml = { "<number>" . $<__VALUE__> . "</number>\n" }
factor:     $/.xml = { Kid::XML::emit_factor( $<child>.xml ); }

term:       $/.xml = { $<term>.xml . Kid::XML::emit_op( $<op> ) . $<factor>.xml }
expression: $/.xml = { Kid::XML::emit_expr( $<expression>.xml, $<op>, $<term>.xml ) }

nil:        $/.xml = { '' }
identifier: $/.xml = { "<identifier>" . $<__VALUE__> . "</identifier>\n" }
var:        $/.xml = { "<var>\n" . $<identifier>.xml . "</var>\n" }

assignment: $/.xml = { Kid::XML::emit_assign( $<var>.xml, $<expression>.xml ) }

block:           $/.xml = { Kid::XML::emit_block( $<statement_list>.xml ); }
else_block:      $/.xml = { $<block>.xml }
rhs_expression:  $/.xml = { $<expression>.xml }

rel_op:       $/.xml = { "<rel_op>" . Kid::XML::escape( $<__VALUE__> ) . "</rel_op>\n" }
condition:    $/.xml = { Kid::XML::emit_cond( $<expression>.xml, $<rel_op>.xml, $<rhs_expression>.xml ) }
if_statement: $/.xml = { Kid::XML::emit_if( $<condition>.xml, $<block>.xml, $<else_block>.xml ) }

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
