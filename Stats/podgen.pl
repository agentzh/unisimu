#: astgen.pl
#: Generate an AST from Perl script samples for
#: ast2pod.pl or such.
#: StatSim v0.04
#: Copyright Agent Zhang. All rights reserved.
#: 2005-06-19 2005-06-19

use strict;
use warnings;

use Getopt::Std;
#use Agent2002::UIUtil;
#use Data::Dumper;
use YAML::Syck;
#use File::Spec;
use Template;

my %opts;
getopts("n:a:e:t:o:", \%opts);
my $name = $opts{n};
my $author = $opts{a};
my $email = $opts{e};
my $ttfile = $opts{t};
my $outfile = $opts{o};

$outfile = "$name.pod" if !$outfile;
#$outfile = File::Spec->rel2abs($outfile);

die "No template specified.\n" if !$ttfile;
#$ttfile = File::Spec->rel2abs($ttfile);

my $out;
my $fname;
if ($name) {
    $fname = "$name.ast";
    open $out, ">$fname" or
        die "error: Cannot open $fname for writing: $!\n";
} else {
    $name = 'summary';
    $out = *STDOUT;
}

my @files = sort map glob, @ARGV;
#die "@files\n";
die "error: No input file specified.\n"
    unless @files;

sub output {
    print $out @_;
}

my @samples;

foreach my $file (@files) {
    #warn "$file\n" if $opts{n};
    #unless ($file =~ m/\.pl$/) {
    #    warn "non-PL file $file skipped.\n";
    #    next;
    #}
    my $ymlfile = "$file.yml";
    die "$ymlfile not found.\n" if !-e $ymlfile;
    push @samples, LoadFile($ymlfile);
}

my $vars = {
    name => $name,
    author => $author,
    email => $email,
    samples => \@samples,
};

my $tt = Template->new({ ABSOLUTE => 1, RELATIVE => 1 });
$tt->process($ttfile, $vars, $outfile)
      || die $tt->error();
