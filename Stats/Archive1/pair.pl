#: pair.pl
#: Example L of Section 1.4, Page 14-15
#: Assigning pairs to courts.
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

foreach my $sol (@Res) {
    my @pairs = @$sol;
    @pairs = map { '('.$_->[0].', '.$_->[1].')' } @pairs;
    warn join(' ', @pairs), "\n";
}
warn "\n";
print "There are ", scalar(@Res), " assignments and the result ",
    fac(2*$m)/2**$m, " is expected.\n";

sub fac { # Compute the factorial of n, say, n!
    my $n = shift;
    return 1 if $n == 0;
    my $res = 1;
    foreach my $i (2..$n) {
        $res *= $i;
    }
    return $res;
}

__END__

Example L

In how many ways can n = 2m people be paired and assigned to m courts
for the first round of a tennis tournament?

In this problem, n_i = 2, i = 1, ..., m, and, according to Proposition
C, there are

    (2m)! / 2^m

assignments.
