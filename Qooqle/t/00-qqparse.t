use strict;
use warnings;
use Test::More tests => 3 * 2;
use Test::Differences;
use File::Slurp;

my @cmd = ($^X, 'bin/qqparse.pl');

for (glob 't/logs/*/*.txt') {
    test_on_file($_);
}

sub test_on_file {
    my $infile = shift;
    my $outfile = "$infile.data";
    unlink $outfile if $outfile;
    is system(@cmd, $infile), 0, "qqparse.pl $infile ok";
    ok -f $outfile, "$outfile generated";
    if (-f $outfile) {
        my $got = read_file($outfile);
        my $expected = read_file($outfile . "2");
        eq_or_diff $got, $expected,   "testing strings";
    }
}
