#: fast.pl
#: Flowchar AST structuralization tool
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-10 2006-03-10

use strict;
use warnings;

use FAST;
use Getopt::Std;

my %opts;
getopts('h', \%opts);

if ($opts{h}) { Usage(0); }

my $infile = shift;
if (not $infile) { Usage(1); }

my $g = FAST->new($infile) or die FAST::error;

my $out;

print "Generating original flowchart graphical output...\n";
my $outfile = "$infile.png";
$g->as_png($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating original flowchart assembly-like output...\n";
$outfile = "$infile.asm";
$g->as_asm($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

my $ast = $g->structured(optimized => 0) or die FAST::error;

print "Generating graphical output for the (unoptimized) structuralized program...\n";
$outfile = "$infile.unopt.png";
$ast->as_png($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating C-like code dump for the (unoptimized) structuralized program...\n";
$outfile = "$infile.unopt.c";
open $out, "> $outfile" or
    die "Can't open $outfile for writing: $!";
print $out $ast->as_c();
close $out;
print "  `$outfile' generated.\n" if -f $outfile;

$ast = $g->structured(optimized => 1) or die FAST::error;

print "Generating graphical output for the (optimized) structuralized program...\n";
$outfile = "$infile.opt.png";
$ast->as_png($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating C-like code dump for the (optimized) structuralized program...\n";
$outfile = "$infile.opt.c";
open $out, "> $outfile" or
    die "Can't open $outfile for writing: $!";
print $out $ast->as_c();
close $out;
print "  `$outfile' generated.\n" if -f $outfile;

sub Usage {
    my $retval = shift;
    my $msg = "Usage: fplot <infile> ...\n";
    if ($retval == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($retval);
    }
}
