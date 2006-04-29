#: Kid/Logic/Disjoint.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-29

package Kid::Logic::Disjoint;

use strict;
use warnings;

#use Data::Dumper::Simple;
use Kid;
use Kid::Proc;
use Language::AttributeGrammar;
use Kid::Logic;
use Clone;

our $Grammar;

our %ReverseOp = (
    '>'  => '<=',
    '>=' => '<',
    '<'  => '>=',
    '<=' => '>',
    '='  => '<>',
    '<>' => '=',
);

sub transform {
    my $logic_ast = shift;
    emit_disjoint($logic_ast);
}

sub emit_disjoint {
    my $logic_ast = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

And:  $/.space = { Kid::Logic::Disjoint::inner_join( $<first>.space, $<second>.space ) }
Or:   $/.space = { Kid::Logic::Disjoint::outer_join( $<first>.space, $<second>.space ) }
Not:  $/.space = { Kid::Logic::Disjoint::emit_not( $<operand> ) }
Atom: $/.space = { Kid::Logic::Disjoint::emit_atom( $<__VALUE__> ); }
nil:  $/.space = { [[]] }

END_GRAMMAR
    $Grammar->apply($logic_ast, 'space');
}

sub inner_join {
    my ($first, $second) = @_;
    my @res;
    for my $x (@$first) {
        for my $y (@$second) {
            push @res, [ @{ Clone::clone($x) }, @{ Clone::clone($y) } ];
        }
    }
    return \@res;
}

sub outer_join {
    my ($first, $second) = @_;
    [ @$first, @$second ];
}

sub emit_not {
    my ($atom) = @_;
    my $cond = $atom->value;
    die ref $cond if !ref $cond or ref $cond ne 'condition';
    my $rel_op = $cond->rel_op;
    $rel_op->value( $ReverseOp{ $rel_op->value } );
    undef $cond->{__PARANT__};
    [[ Clone::clone($cond) ]];
}

sub emit_atom {
    my $value = shift;
    undef $value->{__PARENT__};
    [[ Clone::clone( $value ) ]]
}

sub translate {
    my $src = $_[0];
    #warn $src;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    my $ast = Kid::Proc::transform($ptree);
    my $logic_ast = Kid::Logic::transform($ast);
    my $disjoint_ast = transform($logic_ast);
    my @ands;
    for my $conj (@$disjoint_ast) {
        #$Data::Dumper::Indent = 1;
        my $args = join ' ', map { Kid::Logic::emit_atom($_); } @$conj;
        push @ands, "(and $args)";
    }
    "(or\n" . join( "\n", @ands ) . "\n)\n";
}

1;
__END__
