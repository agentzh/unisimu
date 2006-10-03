#: groups2.pl
#: Example K of Section 1.4, Page 14
#: Require:
#:   Set::Scalar
#:   Math::Combinatorics
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-07 2005-03-30

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

genres [0..8], [2, 4, 3];

my @tags = qw(A G C);
foreach my $sol (@Res) {
    my @seq;
    my @groups = @$sol;
    for (my $i = 0; $i < @groups; $i++) {
        my $tag = $tags[$i];
        map { $seq[$_] = $tag } @{$groups[$i]};
    }
    warn @seq, "\n";
}
warn "\n";
print "There are total ", scalar(@Res), " solutions\n";

__END__

Example K

In how many ways can the set of nucleotides

    { A, A, G, G, G, G, C, C, C }

be arranged in a sequence of nine letters? Proposition C can be
applied by realizing that this problem can be cast as determining
the number of ways that the nine positions in the sequence can be
divided into subgroups of sizes two, four, and three (the locations
and the letters A, G, and C):

        9
    (       ) = 9! / (2! 4! 3!) = 1260
      2 4 3
