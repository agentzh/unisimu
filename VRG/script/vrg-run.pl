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
getopts('tv', \%opts) or help();
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

my $ext;
if ($infile =~ /\.(\w+)$/) { $ext = lc($1); }

if ($ext eq 'vrg') {
    if (system("$^X script/vrgs.pl $infile") != 0) {
        die "Can't compile $infile down to XClips code";
    }
    $infile =~ s/\.vrg/.xclp/i;
    $ext = 'xclp';
}
if ($ext eq 'xclp') {
    if (system("$^X script/xclips.pl -I knowledge $infile") != 0) {
        die "Can't compile $infile down to CLIPS code";
    }
    $infile =~ s/\.xclp/.clp/i;
    $ext = 'clp';
}
if ($ext ne 'clp') {
    die "error: unknown output file format: $ext\n";
}
#warn $infile;

my $clips = Clips::Batch->new(
    'knowledge/vectorize.clp',
    $infile,
    'knowledge/anti-vectorize.clp',
    'knowledge/vector-eval.clp'
);

$clips->watch('rules') if !$opts{t};
$clips->watch('facts') if !$opts{t};

$clips->reset;

$clips->focus('Vectorize');
$clips->facts('*', \my $vect_init_facts) if !$opts{t};

$clips->rules('*') if !$opts{t};
#$clips->facts('*', \my $init_facts);

$clips->run(\my $vect_run_log);
$clips->facts('Eval', \my $vect_facts);

$clips->focus('Eval');
$clips->facts('*', \my $eval_init_facts) if !$opts{t};
$clips->run(\my $eval_run_log);
$clips->facts('Eval', \my $eval_facts);

$clips->focus('AntiVectorize');
$clips->facts('*', \my $anti_init_facts);
$clips->run(\my $anti_run_log);
$clips->facts(\my $anti_vec_facts);

$clips->eof;
#warn "FACTS: ", $facts;

#warn "GOAL!!! $goal_res\n";

my $goal;

if ($anti_init_facts =~ /\(contradiction (\S+) (\S+)\)/) {
    print "Contradiction detected. (Check the relationships between $1 and $2.)\n";
} elsif ($anti_vec_facts =~ /\(goal ([^\)]+)\)/) {
    $goal = "($1)";
    #warn "goal: $1\n";
    my $pat = quotemeta($goal);
    if ($anti_vec_facts =~ /\s+$pat\s*\n/s) {
        print "Yes.\n";
    } else {
        print "No.\n";
    }
} else {
    warn "no goal found.\n";
}

if ($opts{t}) {
    while ($vect_facts =~ /\(vector-relation ([^\)]+)\)/g) {
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
} else {
    my $fname = "vectorize.png";
    my $painter = Clips::GraphViz->new($vect_init_facts, $vect_run_log);
    $painter->draw(
        outfile     => $fname,
        fact_filter => \&format_fact,
        trim => 1,
    );
    warn "generating $fname...\n";

    $fname = "vector-eval.png";
    $painter = Clips::GraphViz->new($eval_init_facts, $eval_run_log);
    $painter->draw(
        outfile     => $fname,
        fact_filter => \&format_fact,
        trim => 1,
    );
    warn "generating $fname...\n";

    $fname = "anti-vectorize.png";
    $painter = Clips::GraphViz->new($anti_init_facts, $anti_run_log);
    $painter->draw(
        outfile     => $fname,
        fact_filter => \&format_fact,
        trim => 1,
    );
    warn "generating $fname...\n";

    $infile =~ s/\.clp//i;
    $fname = "$infile.png";
    $painter = Clips::GraphViz->new(
        $vect_init_facts,
        $vect_run_log . $eval_run_log . $anti_run_log);
    $painter->draw(
        outfile     => $fname,
        fact_filter => \&format_fact,
        trim => 1,
        goal => $goal,
    );
    warn "generating $fname...\n";
}

sub help {
    die <<"_EOC_";
Usage: $0 [-vt] infile
Options:
    -v    verbose mode (prints out CLIPS sessions)
    -t    test mode (for unit testing only)

_EOC_
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
