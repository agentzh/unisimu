#: Kid/Logic.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-29

package Kid::Logic;

use strict;
use warnings;

#use Data::Dumper::Simple;
use Kid;
use Kid::Proc;
use Language::AttributeGrammar;
use Kid::Maple;
#use Clone;

our ($TransGrammar, $TextGrammar);

sub transform {
    my $parse_tree = shift;
    emit_logic($parse_tree);
}

sub emit_logic {
    my $parse_tree = shift;
    $TransGrammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

assignment: $/.logic = { Atom->new( $/ ); }

nil:         $/.logic = { nil->new; }
block:       $/.logic = { $<statement_list>.logic }

else_statement:  $/.logic = { $<statement>.logic }

condition:    $/.logic = { Atom->new( $/ ); }
if_statement: $/.logic = { Kid::Logic::emit_if( $<condition>.logic, $<statement>.logic, $<else_statement>.logic ); }

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
    my $ptree = $parser->program($src) or return undef;
    my $ast = Kid::Proc::transform($ptree);
    my $logic_ast = transform($ast);
    $TextGrammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

And:  $/.text = { Kid::Logic::emit_group( 'and', $<first>.text, $<second>.text ); }
Or:   $/.text = { Kid::Logic::emit_group( 'or',  $<first>.text, $<second>.text ); }
Not:  $/.text = { "(not " . $<operand>.text . ")" }
Atom: $/.text = { Kid::Logic::emit_atom( $<__VALUE__>.text ); }
nil:  $/.text = { '' }
assignment: $/.text = { $/ }
condition:  $/.text = { $/ }

END_GRAMMAR
    $TextGrammar->apply($logic_ast, 'text') . "\n";
}

sub emit_group {
    my ($name, $first, $second) = @_;
    if (! $second) {
        return $first;
    }
    if (! $first) {
        return $second;
    }
    "($name $first $second)";
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
    bless { __VALUE__ => $child }, $class;
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
