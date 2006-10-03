#: plate2.pl
#: Example D of Section 1.4, Page 10.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang
#: 2005-3-31 2005-3-31

use strict;
use warnings;

my @Res;
sub genres {
    my ($rplaces, $rselected, $buf) = @_;
    my @places = @$rplaces;
    my %selected;
    unless (defined $buf) {
        $buf = '';
        %selected = ();
        @Res = ();
    } else {
        %selected = %$rselected;
    }
    my $pos = length($buf);
    if ($pos == @places) {
        push @Res, $buf;
        return;
    }
    if ($places[$pos] eq 'Number') {
        foreach my $e (0..9) {
            next if $selected{$e};
            $selected{$e} = 1;
            genres($rplaces, \%selected, $buf.$e);
            delete $selected{$e};
        }
    } elsif ($places[$pos] eq 'Letter') {
        foreach my $e ('A'..'Z') {
            next if $selected{$e};
            $selected{$e} = 1;
            genres($rplaces, \%selected, $buf.$e);
            delete $selected{$e};
        }
    } else {
        die "$places[$pos] at $pos";
    }
}

genres [qw(Letter Letter Number Number)];
$" = "\n";
#warn "@Res\n\n";
print "The probability that the license plate for a\n",
      "new car will contain no duplicate letters or\n",
      "numbers is ", scalar(@Res), "/67600 = ", trim(@Res/67600),
      "\n(", 26*25*10*9, '/', 26**2 * 10**2, " expected).\n";

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example D

If all sequence of six characters (three letters followed by three
numbers) are equally likely, what is the probability that the license
plate for a new car will contain no duplicate letters or numbers?

Call the desired event A; Omega consists of all 17,576,000 possible
sequences. Since these are all equally likely, the probability of A
is the ratio of the number of ways that A can occur to the total number
of possible outcomes. There are 26 choices for the first letter, 25
for the second, 24 for the third, and hence 26 x 25 x 24 = 15,600 ways
to choose the letters without duplication (doing so corresponds to
sampling without replacement) and 10 x 9 x 8 = 720 ways to choose
the numbers without duplication. From the multiplication principle,
there are 15,600 x 720 = 11,232,000 nonrepeating sequences. The
probability of A is thus

    P(A) = 11,232,000 / 17,576,000 = .64
