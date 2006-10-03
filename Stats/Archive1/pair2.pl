#: pair.pl
#: Example L of Section 1.4, Page 14-15
#: Without assigning pairs to courts.
#: Agent2002. All rights reserved.
#: 2005-03-07 2005-03-08

use strict;
use warnings;

use Set::Scalar;
use Math::Combinatorics;

my @Res;
sub genres {
    my ($relems, $rclasses, $rbuf) = @_;
    unless ($rbuf) {
        $rbuf = [];
        @Res = ();
    }
    my @classes = @$rclasses;
    my $class = shift @classes;
    unless (defined $class) {
        push @Res, $rbuf;
        return;
    }
    my $combinat = Math::Combinatorics->new(
        count => $class,
        data  => $relems,
    );
    while (my @comb = $combinat->next_combination) {
        genres(
            [subtract($relems, \@comb)],
            [@classes],
            [@$rbuf, [@comb]],
        );
    }
}

sub subtract {
    my ($rA, $rB) = @_;
    my $A = Set::Scalar->new(@$rA);
    my $B = Set::Scalar->new(@$rB);
    my $C = $A - $B;
    return $C->elements;
}

my $m = shift;
$m = 3 unless defined $m;
my $n = 2 * $m;

my @classes;
foreach (1..$m) { push @classes, 2; }

genres [1..$n], \@classes;

my @FinalRes;
foreach my $sol (@Res) {
    my @pairs = @$sol;
    @pairs = map {
        @$_ = sort @$_;
        '('.$_->[0].', '.$_->[1].')';
    } @pairs;
    @pairs = sort @pairs;
    push @FinalRes, join(' ', @pairs);
}
@FinalRes = unique(@FinalRes);

$" = "\n";
print "@FinalRes\n";
print "\nThere are ", scalar(@FinalRes), " assignments and the result ",
    fac(2*$m)/(fac($m) * (2**$m)), " is expected.\n";

sub fac { # Compute the factorial of n, say, n!
    my $n = shift;
    return 1 if $n == 0;
    my $res = 1;
    foreach my $i (2..$n) {
        $res *= $i;
    }
    return $res;
}

sub unique {
    my @list = sort @_;
    my (@retval, $prev);
    foreach my $elem (@list) {
        next if $prev and $elem eq $prev;
        push @retval, $elem;
        $prev = $elem;
    }
    return @retval;
}

__END__

Example L

In how many ways can n = 2m people be paired and assigned to m courts
for the first round of a tennis tournament?

In this problem, n_i = 2, i = 1, ..., m, and, according to Proposition
C, there are

    (2m)! / 2^m

assignments.

One has to be careful with problems such as this one. Suppose it were
asked how many ways 2m people could be arranged in pairs without
assigning the pairs to courts. Since there are m! ways to assign the m
pairs to m courts, the preceding result should be divided by m!, giving

    (2m)! / (m! 2^m)

pairs in all.
