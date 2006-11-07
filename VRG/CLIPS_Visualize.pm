package CLIPS::Visualize;

use strict;
use warnings;
use GraphViz;
use Data::Dump::Streamer;

my %NormalNodeStyle =
(
    shape => 'box',
    style => 'filled',
    fillcolor => '#f5f694',
);

my %EdgeStyle =
(
    color => 'red',
);

my %InitArgs = (
    layout => 'dot',
    node => \%NormalNodeStyle,
    edge => \%EdgeStyle,
);

my %FactStyle =
(
    shape => 'ellipse',
    style => 'filled',
    fillcolor => '#c7f77c',
);

sub new ($$$@) {
    my $class = ref $_[0] ? ref shift : shift;
    my ($init_facts, $run_log) = @_;
    #warn $init_facts;
    open my $in, '<', \$init_facts;
    my @init_facts;
    while (<$in>) {
        if (/(?x) ^ f-(\d+) \s+ (\(.*\))/) {
            my ($id, $body) = ($1, $2);
            $init_facts[$id] = $body;
        }
    }
    close $in;

    open $in, '<', \$run_log;
    my @fires;
    my @facts = @init_facts;
    while (<$in>) {
        if (/(?x) ^ FIRE \s+ \d+ \s+ (\S+?): \s+ (.+) $/) {
            my ($rule, $facts) = ($1, $2);
            #warn $facts;
            my @facts = map { s/^f-//; $_ } split /\s*,\s*/, $facts;
            #warn "@facts";
            push @fires, [$rule, \@facts, []];
        }
        elsif (/(?x) ^ ==> \s+ f-(\d+) \s+ (\(.+\))/) {
            my ($id, $body) = ($1, $2);
            push @{ $fires[-1]->[-1] }, $id;
            $facts[$id] = $body;
        }
    }
    bless {
        init_facts => \@init_facts,
        facts      => \@facts,
        fires      => \@fires,
    }, $class;
}

sub draw($$$) {
    my $self = shift;
    my %opts = @_;
    my $fname = $opts{outfile} || 'a.png';
    my $fact_filter = $opts{fact_filter} || sub { $_[0] };
    my $rule_filter = $opts{rule_filter} || sub { "$_[0]\n#$_[1]" };
    my $trim = 1;
    if (exists $opts{trim}) { $trim = $opts{trim} }
    #Dump($self)->Out;
    my @facts = @{ $self->{facts} };
    my @fires = @{ $self->{fires} };
    my $gv = GraphViz->new(%InitArgs);
    my @fact_refs = ();
    for (0..$#fires) {
        next if $trim and @{ $fires[$_][2] } == 0;
        $gv->add_node($_, label => $rule_filter->($fires[$_][0], $_));
        for my $old_fact (@{ $fires[$_][1] }) {
            $fact_refs[$old_fact] = 1 if $trim;
            $gv->add_edge("f-$old_fact" => $_);
        }
        for my $new_fact (@{ $fires[$_][2] }) {
            $fact_refs[$new_fact] = 1 if $trim;
            $gv->add_edge($_ => "f-$new_fact");
        }
    }
    for (0..$#facts) {
        next if !$facts[$_] or ($trim and !defined $fact_refs[$_]);
        $gv->add_node("f-$_", label => $fact_filter->($facts[$_]), %FactStyle);
    }
    $gv->as_png($fname);
}

1;
