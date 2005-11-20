#: Interp/Newton/DiffQuots.pm
#: 用差商法求解牛顿插值公式
#: v0.01
#: Copyright (c) Agent Zhang
#: 2005-11-14 2005-11-18

package Interp::Newton::DiffQuots;

use strict;
use warnings;
use PerlMaple;
use base 'Interp';

sub diff_quot {
    my $self = shift;
    return @{$self->{diff_tb}} if $self->{diff_tb};
    my @x = $self->Xs;
    my @y = $self->Ys;
    my @cols;
    push @cols, \@y;
    for my $i (1..@y-1) {
        #warn "i = $i\n";
        my $max = @y-$i-1;
        my @col = ();
        for my $j (0..$max) {
            #warn "  j = $j\n";
            my @prev_col = @{$cols[$i-1]};
            my $cmd = "(($prev_col[$j+1]) - ($prev_col[$j]))/(($x[$j+$i]) - ($x[$j]))";
            $col[$j] = PerlMaple->eval($cmd);
            #warn "  $cmd = $col[$j]\n";
        }
        push @cols, [@col];
        #warn "col = (@col)\n";
    }
    $self->{diff_tb} = \@cols;
    return @cols;
}

sub polynomial {
    my $self = shift;
    my @diffs = $self->diff_quot;
    my @x = $self->Xs;
    my @terms = $diffs[0][0];
    for my $i (1..@diffs-1) {
        my $term = "$diffs[$i][0]";
        if ($term !~ /^[\d\.]+$/) {
            $term = "($term)";
        }
        for my $j (0..$i-1) {
            my $xi = $x[$j];
            if ($xi !~ /^[\d\.]+$/) {
                $xi = "($xi)";
            }
            $term .= "*(x-$xi)";
        }
        push @terms, $term;
    }
    my $poly = join('+', map { /^\d+$/ ? $_ : "($_)" } @terms);
    #warn $poly;
    return $poly;
}

1;
