use strict;
use warnings;

use CLIPS_Visualize;
use Getopt::Std;
use CLIPS;

my %opts;
getopts('dv', \%opts) or help();
$CLIPS::Verbose = $opts{v};

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

my $infile = shift or help();;
my $clips = CLIPS->new('vectorize.clp', $infile, 'vector-eval.clp');

$clips->watch('rules');
$clips->watch('facts');

$clips->reset;

$clips->focus('Vectorize');

$clips->rules if $opts{v};
#$clips->facts('*', \my $init_facts);

$clips->run(\my $run_log);
$clips->facts('Eval', \my $vectorize_facts);

$clips->focus('Eval');
$clips->facts('*', \my $init_facts);
$clips->run(\$run_log);
$clips->facts('Eval', \my $eval_facts);

$clips->eof;
#warn "FACTS: ", $facts;

while ($vectorize_facts =~ /\(vector-relation ([^\)]+)\)/g) {
    print format_fact($&), "\n";
}

print "---\n";

while ($eval_facts =~ /\(vector-relation ([^\)]+)\)/g) {
    print format_fact($&), "\n";
}

if ($opts{d}) {
    my $painter = CLIPS::Visualize->new($init_facts, $run_log);
    $painter->draw(
        outfile     => "a.png",
        fact_filter => \&format_fact,
        trim => 1,
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
