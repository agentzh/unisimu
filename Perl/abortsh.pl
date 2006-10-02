use strict;
use warnings;
use Win32;
use Getopt::Std;

my %opts;
getopts('ah', \%opts);

my $me = $ENV{COMPUTERNAME};

if ($opts{h}) {
    die "usage: abortsh       # abort the shutdown process (if any)\n".
        "       abortsh -a 30 # shut down the local machine in 30 sec\n";
}

my $delay = shift || 30;

sub shut_down {
    my $message = shift;
    #remind($message);
    Win32::InitiateSystemShutdown(
        $me, $message, $delay,
        1, 0
    );
}

if ($opts{a}) {
    shut_down('good bye~~~');
} else {
    my $retval = Win32::AbortSystemShutdown($me);
    if (!$retval) {
        print "Failed to abort or nothing to abort.\n";
    } else {
        print "shutdown aborted.\n";
    }
}
