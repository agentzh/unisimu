#: n_queens.pl
#: 2006-05-11 2006-05-11

use strict;
use warnings;

use n_queens;

my $n = shift;
die "usage: n_queens <integer>\n" if !defined $n;
die "error: n is negative.\n" if $n < 0;

my @sols = n::Queens::gen_sols($n);

if (!@sols) {
    print "No solutions found.\n";
} else {
    plot_sols(@sols);
    print "For total ", scalar(@sols), " solutions found.\n";
}

sub plot_sols {
    my @sols = @_;
    for my $sol (@sols) {
        for my $i (0..$n-1) {
            for my $j (0..$n-1) {
                if ($sol->[$j] == $i) {
                    print " *";
                } else {
                    print " -";
                }
            }
            print "\n";
        }
        print "\n===\n\n";
    }
}
