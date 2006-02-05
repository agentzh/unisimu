use strict;
use warnings;
use Perl::MinimumVersion;

foreach (@ARGV) {
    print "$_:  ", Perl::MinimumVersion->new( $filename )->minimum_version, "\n";
}
