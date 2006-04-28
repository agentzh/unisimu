#: Kid/Perl.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-22 2006-04-27

package Kid::Perl;

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
    return emit_perl($ast);
}

sub emit_perl {
    my $ast = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

program:    $/.perl = { $<statement_list>.perl }

statement_list: $/.perl = { $<statement_list>.perl . $<statement>.perl }
statement:      $/.perl = { $<child>.perl }
declaration:    $/.perl = { $<child>.perl }

if_statement: $/.perl = { Kid::Perl::emit_if( $<condition>.perl, $<statement>.perl, $<else_statement>.perl ); }

condition:    $/.perl = { $<expression>.perl . $<rel_op>.perl . $<rhs_expression>.perl }
rel_op:       $/.perl = { Kid::Perl::emit_rel_op( $<__VALUE__> ) }
rhs_expression:  $/.perl = { $<expression>.perl }

block:           $/.perl = { "{\n" . $<statement_list>.perl . "}\n" }
else_statement:  $/.perl = { Kid::Perl::emit_else( $<statement>.perl ) }

assignment: $/.perl = { $<var>.perl . '=' . $<expression>.perl . ";\n" }

var:        $/.perl = { '$'.$<identifier>.perl }
identifier: $/.perl = { $<__VALUE__> }
identifier_list: $/.perl = { Kid::Perl::emit_ids( $<identifier_list>.perl, $<identifier>.perl ) }

expression: $/.perl = { $<expression>.perl . $<op> . $<term>.perl }
term:       $/.perl = { $<neg>.perl . $<term>.perl . $<op> . $<factor>.perl }
neg:        $/.perl = { '-' }
factor:     $/.perl = { Kid::Perl::emit_factor($<child>.perl) }
number:     $/.perl = { $<__VALUE__> }

nil:        $/.perl = { '' }

END_GRAMMAR
    return $Grammar->apply($ast, 'perl');
}

sub emit_ids {
    my ($a, $b) = @_;
    if ($a) {
        "$a,$b";
    } else {
        $b;
    }
}

sub emit_factor {
    my $s = shift;
    return (looks_like_number($s) || $s =~ /^\(.*\)$/ || $s =~ /^\$\w+$/) ? $s : "($s)";
}

sub emit_if {
    my ($cond, $then, $else) = @_;
    if ($then !~ /^{.*}$/s) { $then = "{\n$then}\n"; }
    if ($else) {
        return "if($cond)${then}else${else}";
    } else {
        return "if($cond)$then";
    }
}

sub emit_else {
    my ($stmt) = @_;
    return '' if !defined $stmt;
    if ($stmt =~ /^{.*}$/s) {
        $stmt;
    } else {
        "{\n$stmt}\n";
    }
}

sub emit_rel_op {
    my ($op) = @_;
    if ($op eq '=') { return '=='; }
    if ($op eq '<>') { return '!='; }
    $op;
}

1;
__END__
