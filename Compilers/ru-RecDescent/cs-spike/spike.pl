#: spike.pl
#: 2006-05-25 2006-05-26

use strict;
use warnings;

use Getopt::Std;
use FindBin;
use lib $FindBin::Bin;

use spike_parser;
use spike_emitter;

sub help {
    die "Usage: spike [-m] [-n <package-name>] <grammar-file>.\n";
}

my %opts;
getopts('mn:o:', \%opts) or help();

my $infile = shift or help();

my $is_module = $opts{m};

my $package = $opts{n} || 'Parser';

open my $in, $infile or
    die "Can't open $infile for reading: $!\n";
my $src;
{ local $/; $src = <$in>; }
close $in;

my $parser = Spike::Parser->new;
my $ast = $parser->parse($src, $infile);
defined $ast or die "Bad grammar!\n";

my $code = Spike::Emitter->emit($ast, $is_module, $package);
defined $code or die "Can't emit parser!\n";

my $outfile;
if ($opts{o}) {
    $outfile = $opts{o};
} else {
    $outfile = $infile;
    if ($outfile !~ s/\.grammar$/.cs/) {
        $outfile .= ".cs";
    }
}
open my $out, "> $outfile" or
    die "Can't open $outfile for writing: $!\n";
print $out $code;
close $out;
print "$outfile generated.\n";
