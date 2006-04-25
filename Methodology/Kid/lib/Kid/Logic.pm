#: Kid/Logic.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-26

package Kid::Logic;

use strict;
use warnings;

#use Data::Dumper::Simple;
use Kid;
use Language::AttributeGrammar;
use Kid::Maple;
use Clone;

our ($TransGrammar, $TextGrammar);

sub transform {
    my $parse_tree = shift;
    emit_logic($parse_tree);
}

sub emit_logic {
    my $parse_tree = shift;
    $TransGrammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

assignment: $/.logic = { Atom->new( $/ ); }

nil:         $/.logic = { '' }
block:       $/.logic = { $<statement_list>.logic }
else_block:  $/.logic = { $<block>.logic }

condition:    $/.logic = { Atom->new( $/ ); }
if_statement: $/.logic = { Kid::Logic::emit_if( $<condition>.logic, $<block>.logic, $<else_block>.logic ); }

statement:      $/.logic = { $<child>.logic }
statement_list: $/.logic = { Kid::Logic::emit_stmt_list( $<statement_list>.logic, $<statement>.logic ); }

program:    $/.logic = { $<statement_list>.logic }

END_GRAMMAR
    return $TransGrammar->apply($parse_tree, 'logic');
}

sub emit_if {
    my ($cond, $then, $else) = @_;
    if ($else) {
        Or->new(
            And->new($cond, $then),
            And->new(Not->new($cond), $else),
        );
    } else {
        Or->new(
            And->new($cond, $then),
            Not->new($cond),
        );
    }
}

sub emit_stmt_list {
    my ($list, $stmt) = @_;
    if ($list) {
        And->new($list, $stmt);
    } else {
        $stmt;
    }
}

sub translate {
    my $src = $_[0];
    #warn $src;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $parse_tree = $parser->program($src) or return undef;
    my $logic_ast = transform($parse_tree);
    $TextGrammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

And:  $/.text = { "(and " . $<first>.text . " " . $<second>.text . ")" }
Or:   $/.text = { "(or "  . $<first>.text . " " . $<second>.text . ")" }
Not:  $/.text = { "(not " . $<operand>.text . ")" }
Atom: $/.text = { Kid::Logic::emit_atom( $<__VALUE__> ); }

END_GRAMMAR
    $TextGrammar->apply($logic_ast, 'text') . "\n";
}

sub emit_atom {
    my $maple = Kid::Maple::emit_maple(@_);
    $maple =~ s/;\n//gs;
    $maple;
}

package Atom;
use base 'Kid::AST::Element';

sub new {
    my ($class, $child) = @_;
    bless { __VALUE__ => Clone::clone( $child ) }, $class;
}

package Binary;
use base 'Kid::AST::Element';

sub new {
    my ($class, $first, $second) = @_;
    bless {
        first  => $first,
        second => $second,
    }, $class;
}

@And::ISA = 'Binary';
@Or::ISA  = 'Binary';

package Not;
use base 'Kid::AST::Element';

sub new {
    my ($class, $operand) = @_;
    bless { operand => $operand }, $class;
}

1;
__END__