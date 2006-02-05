#: minver.pl
#: Determine the minimum version of perl the specified files require
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-05 2006-02-05

use strict;
use warnings;
use Perl::MinimumVersion;

if (not @ARGV) {
    die "usage: minver <.pl file> <.pm file> ...\n";
}

foreach (@ARGV) {
    print "$_ requires perl ", min_ver($_) , ".\n";
}

sub min_ver {
    my $filename = shift;
    my $obj = Perl::MinimumVersion->new( $filename );
    return $obj->minimum_version;
}
