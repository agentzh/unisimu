#: Pod-Extend.t
#: 2005-12-17 2005-12-17

use strict;
use Test::More tests => 9;
use File::Compare 'compare_text';
BEGIN { use_ok('Pod::Extend') };

my $Debug = 0;

my $parser = Pod::Extend->new;
ok $parser;
isa_ok $parser, 'Pod::Extend';

my ($infile, $outfile) = ('t/01test.podx', 't/01test.pod');
ok $parser->parse_from_file($infile, $outfile);
is compare_text($outfile, 't/~01test.pod'), 0;
unlink $outfile if not $Debug;

($infile, $outfile) = ('t/02test.podx', 't/02test.pod');
ok $parser->parse_from_file($infile, $outfile);
is compare_text($outfile, 't/~02test.pod'), 0;
unlink $outfile if not $Debug;

my $dbfile = 't/03test.db';
unlink $dbfile if -f $dbfile;
$Pod::Extend::dbi_dsn = "dbi:SQLite:dbname=$dbfile";
($infile, $outfile) = ('t/03test.podx', 't/03test.pod');
ok $parser->parse_from_file($infile, $outfile);
is compare_text($outfile, 't/~03test.pod'), 0;
unlink $outfile if not $Debug;
unlink $dbfile if not $Debug;
