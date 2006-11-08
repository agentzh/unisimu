#: vrgs - VRG script compiler targeting XCLIPS

use strict;
use warnings;

use FindBin;
use lib "$FindBin::Bin/../lib";
use VRG::Compiler;
use List::MoreUtils 'uniq';
use File::Slurp;
#use Data::Dump::Streamer;

our $infile = shift or
    die "usage: $0 infile\n";
(my $outfile = $infile) =~ s/\.vrg$/.xclp/i;
$outfile .= '.xclp' if $outfile !~ /\.xclp$/i;
#warn "$outfile";

my $source = read_file($infile);

#$::RD_HINT = 1;
#$::RD_TRACE = 1;
our $parser = VRG::Compiler->new;
my $data = $parser->program($source);
if (!defined $data) {
    die "abort.\n";
}
#warn $data;

if ($data) {
    write_file($outfile, $data);
    #print $data;
}
