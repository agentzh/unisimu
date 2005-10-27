#: slice_disp.pl
#: Process dispatchment simulation using
#:   time slice alternation
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-10-11 2005-10-11

use strict;
use warnings;
use Text::Table;
use Getopt::Std;

my %opts;
getopts('db', \%opts);
my ($debug, $batch) = ($opts{d}, $opts{b});

my @PCB;
for (1..5) {
    push @PCB, {
        pid    => "P$_",
        req_tm => 3+$_,
        run_tm => 0,
        status => 'R',
    };
}

my @End;
while (@PCB) {
    my $p = shift @PCB;
    if ($p->{req_tm} > $p->{run_tm}) {
        print "Running $p->{pid}...\n";
        $p->{run_tm}++;
        if ($p->{req_tm} > $p->{run_tm}) {
            push @PCB, $p;
            next;
        }
    }
    print "Terminating $p->{pid}...\n";
    $p->{status} = 'E';
    push @End, $p;
} continue {
    if ($debug) {
        print "------------ PCB List ----------------\n";
        dump_list(@PCB);
        print "\n------------ End List ----------------\n";
        dump_list(@End);
    }
    <STDIN> if !$batch;
    print "\n";
}

sub dump_list {
    my $tb = Text::Table->new(
        "Process ID", "Request Time", "Run Time", "Status"
    );
    $tb->add('-' x 10, '-' x 12, '-' x 8, '-' x 5);
    for (@_) {
        $tb->add($_->{pid}, $_->{req_tm}, $_->{run_tm}, $_->{status});
    }
    print $tb;
}
