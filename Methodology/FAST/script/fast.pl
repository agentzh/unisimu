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

print "Generating original flowchart graphical output...\n";
my $outfile = "$infile.png";
$g->as_png($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating original flowchart assembly-like output...\n";
$outfile = "$infile.asm";
$g->as_asm($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

my $ast = FAST->structured(optimized => 0);

print "Generating graphical output for the (unoptimized) structuralized program...\n";
$outfile = "$infile.unopt.png";
$ast->as_png($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating C-like code dump for the (unoptimized) structuralized program...\n";
$outfile = "$infile.unopt.c";
$ast->as_c($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

$ast = FAST->structured(optimized => 1);

print "Generating graphical output for the (optimized) structuralized program...\n";
$outfile = "$infile.opt.png";
$ast->as_png($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating C-like code dump for the (optimized) structuralized program...\n";
$outfile = "$infile.opt.c";
$ast->as_c($outfile);
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
