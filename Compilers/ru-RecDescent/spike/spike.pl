#: spike.pl
#: 2006-05-25 2006-05-26

use strict;
use warnings;

use Getopt::Std;

use spike_parser;
use spike_emitter;

my %opts;
getopts('mn:', \%opts);

my $infile = shift or
    die "Usage: spike [-m] [-n <package-name>] <grammar-file>.\n";

my $filetype;
if ($opts{m}) {
    $filetype = 'pm';
} else {
    $filetype= 'pl';
}

my $package = $opts{n} || 'Parser';

open my $in, $infile or
    die "Can't open $infile for reading: $!\n";
my $src;
{ local $/; $src = <$in>; }
close $in;

my $parser = Spike::Parser->new;
my $ast = $parser->parse($src);
defined $ast or die "Bad grammar!\n";

my $code = Spike::Emitter->emit($ast, $filetype, $package);
defined $code or die "Can't emit parser!\n";

my $outfile = $infile;
if ($outfile !~ s/\.grammar$/.$filetype/) {
    $outfile .= ".$filetype";
}
open my $out, "> $outfile" or
    die "Can't open $outfile for writing: $!\n";
print $out $code;
close $out;
print "$outfile generated.\n";
