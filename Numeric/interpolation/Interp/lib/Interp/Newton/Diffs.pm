#: Interp/Newton/Diffs.pm
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-17 2005-11-18

package Interp::Newton::Diffs;

use strict;
use warnings;
use base 'Interp';

sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    my $self = $class->SUPER::new(@_);
    bless $self, $class;
    return undef if not $self->equi_step;
    return $self;
}

sub diffs {
    my $self = shift;
    return @{$self->{_diffs}} if $self->{_diffs};
    my @Xs = $self->Xs;
    my @Ys = $self->Ys;
    my $n = @Xs - 1;
    my @cols = \@Ys;
    for my $m (1..$n) {
        #warn "m = $m (n = $n)\n";
        push @cols, [];
        for my $i (0..$n-$m) {
            #warn "  i = $i\n";
            $cols[$m][$i] = PerlMaple->eval(
                "($cols[$m-1][$i+1]) - ($cols[$m-1][$i])");
        }
    }
    $self->{_diffs} = \@cols;
    return @cols;
}

sub raw_poly {
    my $self = shift;
    my $h = $self->step;
    my @diffs = $self->diffs;
    my @Xs = $self->Xs;
    my $n = @Xs - 1;
    my @terms;
    for my $k (0..$n) {
        #warn "  ".($diffs[$k][0])."/$k!\n";
        my @factors = "($diffs[$k][0])/$k!";
        for my $j (0..$k-1) {
            push @factors, "(t-$j)";
        }
        push @terms, join('*', @factors);
    }
    return "N($Xs[0]+($h)*t)=".join('+', @terms);
}

sub step {
    return shift->{_h};
}

sub equi_step {
    my $self = shift;
    my @Xs = $self->Xs;
    #@Xs = sort @Xs;
    return 1 if @Xs < 2;
    my $h = "($Xs[1])-($Xs[0])";
    $self->{_h} = PerlMaple->eval($h);
    #warn "  h = $h\n";
    for my $i (1..@Xs-2) {
        #warn " i = $i; $Xs[$i], $Xs[$i+1]\n";
        if (PerlMaple->eval("testeq( ($Xs[$i+1]) - ($Xs[$i]), $h )") eq 'false') {
            #warn "  testeq( ($Xs[$i+1]) - ($Xs[$i]), $h )";
            return undef;
        } else {
            #warn "testeq( ($Xs[$i+1]) - ($Xs[$i]), $h ) : ", PerlMaple->error;
        }
    }
    return 1;
}

sub polynomial {
    my $self = shift;
    my $equ = $self->raw_poly;
    my $poly = PerlMaple->eval("rhs($equ)");
    my @Xs = $self->Xs;
    my $h = $self->step;
    return PerlMaple->eval("eval($poly, t=(x-($Xs[0]))/($h))");
}

1;
