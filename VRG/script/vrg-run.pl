use strict;
use warnings;

use FindBin;
use lib "$FindBin::Bin/../lib";
use Clips::Batch;
use Clips::GraphViz;
use XClips::Compiler;
use File::Slurp;
use Getopt::Std;

my %opts;
getopts('dv', \%opts) or help();
$Clips::Batch::Verbose = $opts{v};

my %infix = (
    'parallel'   => '//',
    'orthogonal' => 'T',
    'cross'      => 'X',
    'on'         => 'on',
);

my %prefix = (
    'line'  => '\\\\',
    'plane' => '\\#',
);

my $infile = shift or help();
die "File $infile not found" if !-f $infile;

my $goal = shift;

my $clips = Clips::Batch->new(
    'knowledge/vectorize.clp',
    $infile,
    'knowledge/anti-vectorize.clp',
    'knowledge/vector-eval.clp'
);

$clips->watch('rules') if $opts{v} or $opts{d};
$clips->watch('facts') if $opts{v} or $opts{d};;

$clips->reset;

$clips->focus('Vectorize');

$clips->rules('*') if $opts{v};
#$clips->facts('*', \my $init_facts);

$clips->run(\my $run_log);
$clips->facts('Eval', \my $vectorize_facts);

$clips->focus('Eval');
$clips->facts('*', \my $eval_init_facts);
$clips->run(\my $eval_run_log);
$clips->facts('Eval', \my $eval_facts);

$clips->focus('AntiVectorize');
$clips->facts('*', \my $final_init_facts);
$clips->run(\my $final_run_log);
$clips->facts(\my $anti_vec_facts);

$clips->eof;
#warn "FACTS: ", $facts;

while ($vectorize_facts =~ /\(vector-relation ([^\)]+)\)/g) {
    print format_fact($&), "\n";
}

print "---\n";

while ($eval_facts =~ /\(vector-relation ([^\)]+)\)/g) {
    print format_fact($&), "\n";
}

print "---\n";

while ($anti_vec_facts =~ /\(space-relation ([^\)]+)\)/g) {
    print format_fact($&), "\n";
}

if ($opts{d}) {
    my $painter = Clips::GraphViz->new($eval_init_facts, $eval_run_log);
    $painter->draw(
        outfile     => "eval.png",
        fact_filter => \&format_fact,
        trim => 1,
        goal => $goal,
    );
    $painter = Clips::GraphViz->new($final_init_facts, $final_run_log);
    $painter->draw(
        outfile     => "final.png",
        fact_filter => \&format_fact,
        trim => 1,
        goal => $goal,
    );
}

sub help {
    die "usage: $0 [-vd] infile\n";
}

sub format_fact {
    my $clips = $_[0];
    if ($clips =~ /(?x) ^ \( (space|vector)-relation \s+ (\S+) \s+ (\S+) \s+ (\S+) \) $/) {
        #warn "$1, $2, $3";
        my ($type, $rel, $a, $b) = ($1, $2, $3, $4);
        my $prefix = '';
        if ($rel =~ s/^not_//) {
            $prefix = '~';
        }
        my $infix = $infix{$rel};
        if ($infix) {
            return $type eq 'space' ? "$a [$prefix$infix] $b" : "$a <$prefix$infix> $b";
        }
    }
    elsif ($clips =~ /(?x) ^ \( (line|plane) \s+ (\S+) \) $/) {
        #warn "$1, $2";
        my ($type, $a) = ($1, $2);
        my $prefix = $prefix{$type};
        return "$prefix $a";
    }
    elsif ($clips =~ /(?x) ^ \( (\S+) ((?:\s+ \S+)*) \) $/) {
        my ($func, $args) = ($1, $2);
        $args =~ s/^\s+//g;
        my @args = split /\s+/, $args;
        return "$func(" . join(',', @args) . ')';
    }
    $clips;
}
