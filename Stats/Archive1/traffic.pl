#: traffic.pl
#: Example A of Section 1.2, Page 2
#: Agent2002. All rights reserved.
#: 2005-03-06 2005-03-07

use strict;
use warnings;

my $NELEMS = 3;
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
    genres($buf.'c');
    genres($buf.'s');
}

genres;

{
    local $" = ', ';
    print "Omega = { @Res }\n";
}

__END__

Example A

Driving to work, a commuter passes through a sequence of three 
intersections with traffic lights. At each light, she either 
stops, s, or continues, c. The sample space is the set of all 
possible outcomes:

    Omega = { ccc, ccs, css, csc, sss, ssc, scc, scs }
