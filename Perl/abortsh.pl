use strict;
use warnings;
use Win32;

my $machine = $ENV{COMPUTERNAME};
my $retval = Win32::AbortSystemShutdown($machine);
if (!$retval) {
    print "Failed to abort or nothing to abort.\n";
} else {
    print "done.\n";
}
