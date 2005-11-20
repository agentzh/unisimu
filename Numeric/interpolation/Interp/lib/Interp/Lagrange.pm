#: Interp/Lagrange.pm
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-19 2005-11-19

package Interp::Lagrange;

use strict;
use warnings;
use base 'Interp';

sub polynomial {
    my $self = shift;
    my @Xs = $self->Xs;
    my @Ys = $self->Ys;
    my $n = @Xs - 1;
    my @terms;
    for my $k (0..$n) {
        my @factors = "($Ys[$k])";
        for my $i (0..$n) {
            next if $i == $k;
            my $xi = "($Xs[$i])";
            my $xk = "($Xs[$k])";
            push @factors, "(x-$xi)/($xk-$xi)";
        }
        push @terms, join('*', @factors);
    }
    return join('+', @terms);
}

1;
