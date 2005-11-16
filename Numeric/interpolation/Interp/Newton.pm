#: Interp/Newton.pm
#: 2005-11-14 2005-11-14

package Interp::Newton;

use strict;
use warnings;
use PerlMaple;

my $maple;
BEGIN {
    $maple = PerlMaple->new;
}

sub new {
    my $class = shift;
    my %data = @_;
    my (@x, @y);
    for (sort keys %data) {
        push @x, $_;
        push @y, $data{$_};
    }
    my $self = {
        x => \@x,
        y => \@y,
    };
    return bless $self, $class;
}

sub diff_quot {
    my $self = shift;
    return @{$self->{diff_tb}} if $self->{diff_tb};
    my @x = $self->Xs;
    my @y = $self->Ys;
    my @cols;
    push @cols, \@y;
    for my $i (1..@y-1) {
        warn "i = $i\n";
        my $max = @y-$i-1;
        my @col = ();
        for my $j (0..$max) {
            warn "  j = $j\n";
            my @prev_col = @{$cols[$i-1]};
            my $cmd = "(($prev_col[$j+1]) - ($prev_col[$j]))/($x[$j+$i] - $x[$j])";
            $col[$j] = $maple->eval($cmd);
            warn "  $cmd = $col[$j]\n";
        }
        push @cols, [@col];
        warn "col = (@col)\n";
    }
    $self->{diff_tb} = \@cols;
    return @cols;
}

sub Xs {
    return @{shift->{x}};
}

sub Ys {
    return @{shift->{y}};
}

1;