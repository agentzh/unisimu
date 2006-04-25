#: Kid/Logic/Disjoint.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-25

package Kid::Logic::Disjoint;

use strict;
use warnings;

use Data::Dumper::Simple;
use Kid;
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
Not:  $/.space = { Kid::Logic::Disjoint::emit_not( $<child> ) }
Atom: $/.space = { [[ Clone::clone( $<child> ) ]] }

END_GRAMMAR
    $Grammar->apply($logic_ast, 'space');
}

sub inner_join {
    my ($first, $second) = @_;
    my @res;
    for my $x (@$first) {
        for my $y (@$second) {
            push @res, [ @{ Clone::clone( $x ) }, @{ Clone::clone( $y ) }];
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
    my $cond = $atom->{child};
    die ref $cond if ref $cond ne 'condition';
    reverse_op( $cond->{rel_op}->{__VALUE__} );
    [[ Clone::clone($cond) ]];
}

sub reverse_op {
    $_[0] = $ReverseOp{ $_[0] };
}

sub translate {
    my $src = $_[0];
    #warn $src;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $parse_tree = $parser->program($src) or return undef;
    my $logic_ast = Kid::Logic::transform($parse_tree);
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
