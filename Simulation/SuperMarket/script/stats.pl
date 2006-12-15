use strict;
use warnings;

my @servers;
my @clients;
my $prev_time = 0;
while (<>) {
    if (/^\@(\d+) <Server (\d+)> (<==|==>) Client (\d+)/) {
        my ($time, $server_id, $direction, $client_id) = ($1, $2, $3, $4);
        my $server = $servers[$server_id] ||= {
            queue_len => [],
        };
        my $client = $clients[$client_id] ||= {
            enter => undef,
            start_serving => undef,
            leave => undef,
        };
        if ($direction eq '<==') {
            push @{ $server->{q_len} }, [$time => '++'];
            $client->{enter} = $time;

        } else {
            push @{ $server->{q_len} }, [$time => '--'];
            $client->{leave} = $time;
        }
    }
    elsif (/^@(\d+) <Server (\d+)> serves Client (\d+)/) {
        my ($time, $server_id, $client_id) = ($1, $2, $3);
        $clients[$client_id]->{start_serving} = $time;
    }
    else {
        die "syntax error: line $.: $_\n";
    }
}

for (0..$#servers) {
    my $server = $servers[$_];
    next if !defined $server;
    print "<Server $_>\n";
    my ($accum, $prev_time, $len) = (0, 0, 0);
    for my $item (@{ $server->{q_len} }) {
        my ($time, $op) = @$item;
        eval "\$len$op";
        warn $@ if $@;
        #warn "len = $len\n";
        $accum += ($time - $prev_time) * $len;
        $prev_time = $time;
    }
    print "  Average length of queue: ", $accum / $prev_time, "\n";
}

print "Total\n";

my ($count, $accum);
for (0..$#clients) {
    my $client = $clients[$_];
    next if !defined $client;
    if (defined $client->{enter} and defined $client->{start_serving}) {
        $accum += $client->{start_serving} - $client->{enter};
        $count++;
    }
}
print "  Avarage waiting time: ", $accum / $count, "\n";

($count, $accum) = (0, 0);
for (0..$#clients) {
    my $client = $clients[$_];
    next if !defined $client;
    if (defined $client->{start_serving} and defined $client->{leave}) {
        $accum += $client->{leave} - $client->{start_serving};
        $count++;
    }
}
print "  Avarage serving time: ", $accum / $count, "\n";

($count, $accum) = (0, 0);
for (0..$#clients) {
    my $client = $clients[$_];
    next if !defined $client;
    if (defined $client->{enter} and defined $client->{leave}) {
        $accum += $client->{leave} - $client->{enter};
        $count++;
    }
}
print "  Avarage tarry time: ", $accum / $count, "\n";
