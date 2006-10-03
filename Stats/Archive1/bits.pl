#: bits.pl
#: Example C of Section 1.4.1, Page 9.
#: Agent2002. All rights reserved.
#: 2005-03-09 2005-03-13

use strict;
use warnings;

my $NELEMS = 8;
my @Res;
sub genres {
    my $buf = shift;
    unless (defined $buf) {
        $buf = '';
        @Res = ();
    }
    if (length($buf) == $NELEMS) {
        push @Res, $buf;
        return;
    }
    genres($buf.'0');
    genres($buf.'1');
}

genres;

{
    local $" = "\n";
    warn "@Res\n";
    print "\nThere are ", scalar(@Res),
        " different 8-bit words\n";
}

__END__

Example C

An 8-bit binary word is a sequence of 8 digits, of which
each may be either a 0 or a 1. How many different 8-bit
words are thee?

There are two choices for the first bit, two for the
second, etc., and thus there are

    2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 = 2^8 = 256

such words.
