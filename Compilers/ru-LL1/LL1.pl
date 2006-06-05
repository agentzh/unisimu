#: LL1.pl
#: script for generating LL1 parsers
#: 2006-06-04 2006-06-04

use strict;
use warnings;


use Getopt::Std;
use FindBin;
use lib $FindBin::Bin;
use Data::Dumper::Simple;

use LL1_parser;

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

my $parser = LL1::Parser->new;
my $ast = $parser->parse($src);
defined $ast or die "Bad grammar!\n";

print Dumper($ast);

__END__
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
