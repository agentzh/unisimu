#: fast.pl
#: Flowchar AST structuralization tool
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-10 2006-04-02

use strict;
use warnings;

use FAST;
use Getopt::Std;

my %opts;
getopts('hd', \%opts);

if ($opts{h}) { Usage(0); }

my $infile = shift;
if (not $infile) { Usage(0); }

my $g = FAST->new($infile);

if (! $g) {
    my $error = FAST::error;
    $error =~ s/^FAST::\w+:\s*//;
    die "$error\n";
}
my $out;

print "Generating original flowchart graphical output...\n";
my $outfile;
if ($opts{d}) {
    $outfile = "$infile.dot";
    $g->as_debug($outfile);
} else {
    $outfile = "$infile.png";
    $g->as_png($outfile);
}
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating original flowchart assembly-like output...\n";
$outfile = "$infile.asm";
$g->as_asm($outfile);
print "  `$outfile' generated.\n" if -f $outfile;

my $ast = $g->structured(optimized => 0);

print "Generating graphical output for the (unoptimized) structuralized program...\n";
if ($opts{d}) {
    $outfile = "$infile.unopt.dot";
    $ast->as_debug($outfile);
} else {
    $outfile = "$infile.unopt.png";
    $ast->as_png($outfile);
}
print "  `$outfile' generated.\n" if -f $outfile;

print "Generating C-like code dump for the (unoptimized) structuralized program...\n";
$outfile = "$infile.unopt.c";
open $out, "> $outfile" or
    die "Can't open $outfile for writing: $!";
print $out $ast->as_c();
close $out;
print "  `$outfile' generated.\n" if -f $outfile;

$ast = $g->structured(optimized => 1);

print "Generating graphical output for the (optimized) structuralized program...\n";
if ($opts{d}) {
    $outfile = "$infile.opt.dot";
    $ast->as_debug($outfile);
} else {
    $outfile = "$infile.opt.png";
    $ast->as_png($outfile);
}
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

__END__

=head1 NAME

fast.pl - Driver for FAST

