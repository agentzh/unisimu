use strict;
use warnings;

use FindBin;
use lib "$FindBin::Bin/../lib";
use VRG::GraphViz;
use Clips::Batch;
use Clips::GraphViz;
use XClips::Compiler;
use File::Slurp;
use Getopt::Std;

my %opts;
getopts('tvf', \%opts) or help();
$Clips::Batch::Verbose = $opts{v};

my $cover_test = $ENV{VRG_COVER};
my $no_trim = $opts{f};

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

my ($base, $ext);
if ($infile =~ /([-\w]+)\.(\w+)$/) { $base = $1; $ext = lc($2); }

if ($ext eq 'vrg') {
    if (system("$^X script/vrgs.pl $infile") != 0) {
        die "Can't compile $infile down to XClips code";
    }
    $infile =~ s/\.vrg/.xclp/i;
    $ext = 'xclp';
}
if ($ext eq 'xclp') {
    if (system("xclips -c -I knowledge $infile") != 0) {
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
    'knowledge/vector-eval.clp',
    'knowledge/goal-match.clp',
);

$clips->watch('rules') if !$opts{t} or $cover_test;
$clips->watch('facts') if !$opts{t} or $cover_test;

$clips->reset;

$clips->focus('Vectorize');
$clips->facts('*', \my $vect_init_facts) if !$opts{t};

$clips->rules('*', \my $rule_list) if !$opts{t} or $cover_test;
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

$clips->focus('GoalMatch');
$clips->run(\my $match_run_log);
$clips->facts('GoalMatch', \my $goal_match_facts);

$clips->eof;
#warn "FACTS: ", $facts;

#warn "GOAL!!! $goal_res\n";

my (@solved, @pending, @hint);

#warn $goal_match_facts;

if ($goal_match_facts =~ /\(contradiction (.+)\)/) {
    my $info = $1;
    my @item = split ' ', $info;
    map { s/\"//g } @item;
    print "Contradiction detected: ",
        "$item[2] $item[0] $item[3], $item[2] $item[1] $item[3].\n\n";
} else {
    open my $in, '<', \$goal_match_facts or die;
    while (<$in>) {
        if (/\(solved (.+)\)$/g) {
            push @solved, "($1)";
            #warn "adding solved ($1)";
        }
        elsif (/\(pending (.+)\)$/g) {
            push @pending, "($1)";
            #warn "adding pending ($1)";
        }
        elsif (/\(hint (.+)\)$/g) {
            push @hint, "($1)";
            #warn "adding hint ($1)";
        }
    }
    close $in;

    my @fmt_pending = map { format_fact($_) } @pending;
    my @fmt_hint    = map { format_fact($_) } @hint;
    if (@pending == 0) {
        if (@solved == 0) {
            print "No goal found.\n\n";
        } else {
            print "Yes.\n\n";
        }
    } else {
        if (@solved + @pending == 1) {
            print "No.\n";
        } else {
            print "No.\nPending: ", join(', ', @fmt_pending), "\n";
        }
        if (@hint) {
            print "Hint: ", join(', ', @fmt_hint), "\n";
        }
        print "\n";
    }
}

my $run_log = $vect_run_log . $eval_run_log . $anti_run_log;

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
        trim => $no_trim ? 0 : 1,
    );
    warn "generating $fname...\n";

    $fname = "vector-eval.png";
    $painter = Clips::GraphViz->new($eval_init_facts, $eval_run_log);
    $painter->draw(
        outfile     => $fname,
        fact_filter => \&format_fact,
        trim => $no_trim ? 0 : 1,
    );
    warn "generating $fname...\n";

    $fname = "anti-vectorize.png";
    $painter = Clips::GraphViz->new($anti_init_facts, $anti_run_log);
    $painter->draw(
        outfile     => $fname,
        fact_filter => \&format_fact,
        trim => $no_trim ? 0 : 1,
    );
    warn "generating $fname...\n";

    $infile =~ s/\.clp//i;
    $fname = "$infile.png";
    $painter = Clips::GraphViz->new(
        $vect_init_facts,
        $run_log);
    $painter->draw(
        outfile     => $fname,
        fact_filter => \&format_fact,
        trim => $no_trim ? 0 : 1,
        goals => (@pending == 0 ? \@solved : undef),
    );
    warn "generating $fname...\n";

    $fname = "$infile.vrg1.png";
    $painter = VRG::GraphViz->new($vect_facts);
    $painter->as_png($fname);
    warn "generating $fname...\n";

    $fname = "$infile.vrg2.png";
    $painter = VRG::GraphViz->new($eval_facts);
    $painter->as_png($fname);
    warn "generating $fname...\n";
}

if ($cover_test) {
    require YAML::Syck;
    my $db_dir = 'clips_cover_db';
    mkdir $db_dir if !-d $db_dir;
    my $fname;
    while (my $rand = int rand 1000000) {
        $fname = "$db_dir/$base-$rand.yml";
        last if !-e $fname;
    }
    YAML::Syck::DumpFile($fname, [$rule_list, $run_log . $match_run_log]);
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
