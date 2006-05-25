#: spike.pl
#: 2006-05-25 2006-05-25

use strict;
use warnings;

use spike_parser;
use spike_emitter;

my $infile = shift or
    die "No input file specified.\n";
open my $in, $infile or
    die "Can't open $infile for reading: $!\n";
my $src;
{ local $/; $src = <$in>; }
close $in;

my $ast = Spike::Parser->parse($src);
defined $ast or die "Bad grammar!\n";

my $code = Spike::Emitter->emit($ast);
defined $code or die "Can't emit parser!\n";

my $outfile = $infile;
if ($outfile !~ s/\.grammar$/.pl/) {
    $outfile .= '.pl';
}
open my $out, "> $outfile" or
    die "Can't open $outfile for writing: $!\n";
print $out $code;
close $out;
print "$outfile generated.\n";
