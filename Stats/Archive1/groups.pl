#: groups.pl
#: Example J of Section 1.4, Page 14
#: Require:
#:   Set::Scalar
#:   Math::Combinatorics
#: Agent2002. All rights reserved.
#: 2005-03-07 2005-03-13

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

genres [qw(A B C D E F G)], [3, 2, 2];
foreach my $sol (@Res) {
    my @groups = @$sol;
    @groups = map { '{'.join(', ', @$_).'}' } @groups;
    warn join(' ', @groups), "\n";
}
warn "\n";
print "This can be done in ", scalar(@Res), " ways.\n";

sub subtract {
    my ($rA, $rB) = @_;
    my $A = Set::Scalar->new(@$rA);
    my $B = Set::Scalar->new(@$rB);
    my $C = $A - $B;
    return $C->elements;
}

__END__

Example J

A committee of seven members is to be divided into three
subcommittees  of size three, two, and two. This can be
done in

      7
  (        ) = 7! / (3! 2! 2!) = 210
    3 2 2

ways.
