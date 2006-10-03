#: groups3.pl
#: Example K of Section 1.4, Page 14
#: Another more natural method for this problem
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-07 2005-03-30

use strict;
use warnings;

my $NELEMS;
my @Res;
sub genres {
    my ($rsrc, $buf) = @_;
    my %src = %$rsrc;
    unless (defined $buf) {
        $buf = '';
        @Res = ();
    }
    if (length($buf) == $NELEMS) {
        push @Res, $buf;
        return;
    }
    foreach my $tag (keys %src) {
        next unless $src{$tag} > 0;
        --$src{$tag};
        genres(\%src, $buf.$tag);
        ++$src{$tag};
    }
}

$NELEMS = 9;
genres {A => 2, G => 4, C => 3};
$" = "\n";
warn "@Res\n\n";
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
