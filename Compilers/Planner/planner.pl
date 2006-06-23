#: planner.pl
#: .pln compiler
#: 2006-06-23 2006-06-23

use strict;
use warnings;

use Getopt::Std;
use FindBin;
use lib $FindBin::Bin;
use Data::Dumper::Simple;

use planner_parser;
use planner_emitter;

my %opts;
getopts('mn:', \%opts);

my $infile = shift or
    die "Usage: planner [-m] [-n <package-name>] <grammar-file>.\n";

my $filetype;
if ($opts{m}) {
    $filetype = 'pm';
} else {
    $filetype= 'pl';
}

my $package = $opts{n} || 'Planner';

open my $in, $infile or
    die "Can't open $infile for reading: $!\n";
my $src;
{ local $/; $src = <$in>; }
close $in;

my $parser = Planner::Parser->new;
my $ast = $parser->parse($src);
defined $ast or die "Bad grammar!\n";

#print Dumper($ast);

my $code = Planner::Emitter->emit($ast, $filetype, $package);
defined $code or die "Can't emit code!\n";

my $outfile = $infile;
if ($outfile !~ s/\.pln$/.$filetype/) {
    $outfile .= ".$filetype";
}
open my $out, "> $outfile" or
    die "Can't open $outfile for writing: $!\n";
print $out $code;
close $out;
print "$outfile generated.\n";
