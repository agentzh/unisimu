#: Interp/Newton.pm
#: 2005-11-14 2005-11-14

package Interp::Newton;

use strict;
use warnings;
use PerlMaple;

our $maple;
BEGIN {
    $maple = PerlMaple->new;
}

sub new {
    my $class = shift;
    my %data = @_;
    my (@x, @y);
    if ($data{Xs} and $data{Ys}) {
        @x = @{$data{Xs}};
        @y = @{$data{Ys}};
    } else {
        for (sort keys %data) {
            push @x, $_;
            push @y, $data{$_};
        }
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
        #warn "i = $i\n";
        my $max = @y-$i-1;
        my @col = ();
        for my $j (0..$max) {
            #warn "  j = $j\n";
            my @prev_col = @{$cols[$i-1]};
            my $cmd = "(($prev_col[$j+1]) - ($prev_col[$j]))/($x[$j+$i] - $x[$j])";
            $col[$j] = $maple->eval($cmd);
            #warn "  $cmd = $col[$j]\n";
        }
        push @cols, [@col];
        #warn "col = (@col)\n";
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

sub test_polynomial {
    my $self = shift;
    my $poly = shift;
    my @Xs = $self->Xs;
    my @Ys = $self->Ys;
    foreach (0..@Xs-1) {
        my $res = $Interp::Newton::maple->eval(
            "testeq(eval($poly, x=$Xs[$_]), $Ys[$_])"
        );
        if (!defined $res or $res ne 'true') {
            return undef;
        }
    }
    return 1;
}

sub error {
    return "Maple: ".$maple->error;
}

sub maple {
    return $maple;
}

1;
