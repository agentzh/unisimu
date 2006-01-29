use PPI;
use PPI::Dumper;

use Getopt::Std;

my %opts;
getopts('f:', \%opts);

@ARGV || die "Usage: ppi-dump [-f <file>] [<code>]\n";

my $arg = $opts{f};
$arg ||= \$ARGV[0];

my $dom = PPI::Document->new( $arg );

# Create the dumper
my $dumper = PPI::Dumper->new( $dom );

# Dump the document
$dumper->print;
