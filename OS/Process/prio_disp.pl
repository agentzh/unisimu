#: prio_disp.pl
#: Process dispatchment simulation using
#:   priority list.
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
        prio   => 15-$_,
        status => 'R',
    };
}

my @End;
while (@PCB) {
    my $p = shift @PCB;
    if ($p->{req_tm} > 0) {
        print "Running $p->{pid}...\n";
        $p->{prio}--; $p->{req_tm}--;
        if ($p->{req_tm} > 0) {
            unshift @PCB, $p;
            @PCB = reverse sort {$a->{prio} <=> $b->{prio}} @PCB;
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
        "Process ID", "Request Time", "Priority", "Status"
    );
    $tb->add('-' x 10, '-' x 12, '-' x 8, '-' x 5);
    for (@_) {
        $tb->add($_->{pid}, $_->{req_tm}, $_->{prio}, $_->{status});
    }
    print $tb;
}
