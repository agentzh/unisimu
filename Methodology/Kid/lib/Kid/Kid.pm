#: Kid/Kid.pm
#: A Kid to Kid emitter
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-28 2006-04-29

package Kid::Kid;

use strict;
use warnings;

use Kid;
use Language::AttributeGrammar;
use Scalar::Util qw( looks_like_number );

sub translate {
    my $src = shift;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    return emit($ptree);
}

sub emit {
    my $ptree = shift;
    $Kid::Kid::emit::grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

program:    $/.kid = { $<statement_list>.kid }

statement_list: $/.kid = { $<statement_list>.kid . $<statement>.kid }
statement:      $/.kid = { $<child>.kid }

comment:  $/.kid = { $<__VALUE__> . "\n" }

declaration:     $/.kid = { $<child>.kid }
proc_decl:       $/.kid = { 'proc ' . $<identifier>.kid . '(' . $<identifier_list>.kid . ') ' . $<block>.kid }
identifier_list: $/.kid = { Kid::Kid::emit_list( $<identifier_list>.kid, $<identifier>.kid ) }

if_statement: $/.kid = { Kid::Kid::emit_if( $<condition>.kid, $<statement>.kid, $<else_statement>.kid ); }

condition:    $/.kid = { $<expression>.kid . $<rel_op>.kid . $<rhs_expression>.kid }
rel_op:       $/.kid = { $<__VALUE__> }
rhs_expression:  $/.kid = { $<expression>.kid }

block:           $/.kid = { "{\n" . $<statement_list>.kid . "}\n" }
else_statement:  $/.kid = { $<statement>.kid }

assignment: $/.kid = { $<var>.kid . ':=' . $<expression>.kid . ";\n" }

var:        $/.kid = { $<identifier>.kid }
identifier: $/.kid = { $<__VALUE__> }

expression: $/.kid = { $<expression>.kid . $<op> . $<term>.kid }

expression_list: $/.kid = { Kid::Kid::emit_list( $<expression_list>.kid, $<expression>.kid ); }
term:       $/.kid = { $<neg>.kid . $<term>.kid . $<op> . $<factor>.kid }
neg:        $/.kid = { '-' }
factor:     $/.kid = { Kid::Kid::emit_factor($<child>, $<child>.kid) }
number:     $/.kid = { $<__VALUE__> }
proc_call:  $/.kid = { $<identifier>.kid . '(' . $<expression_list>.kid . ')' }

nil:        $/.kid = { '' }

END_GRAMMAR
    $Kid::Kid::emit::grammar->apply($ptree, 'kid');
}

sub emit_list {
    my ($a, $b) = @_;
    if ($a) {
        "$a,$b";
    } else {
        $b;
    }
}

sub emit_factor {
    my ($child, $s) = @_;
    my $class = ref $child;
    (looks_like_number($s) || $s =~ /^\w*\(.*\)$/ || $s =~ /^\w+$/) ? $s : "($s)";
}

sub emit_if {
    my ($cond, $then, $else) = @_;
    if ($then =~ m/^{/) {
        $then = " $then";
    } else {
        $then = "\n$then";
    }
    if ($else) {
        if ($else =~ m/^(?:{|if )/) {
            $else = " $else";
        } else {
            $else = "\n$else";
        }
        "if ($cond)${then}else${else}";
    } else {
        "if ($cond)$then";
    }
}

1;
__END__
