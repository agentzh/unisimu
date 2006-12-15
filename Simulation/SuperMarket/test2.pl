#!/usr/bin/perl

use strict;
use warnings;
use List::Util 'reduce';

BEGIN { push @INC, 'lib'; }
use SM;

my $count = shift || 10;
my $arrival_time = shift || 2;
my $service_time = shift || 1;

$SM::Client::ArrivalInterval = sub { $arrival_time };
my @servers;
for (1..5) {
    push @servers, SM::Server->new(sub { $service_time });
}
my $handle;
$handle = sub {
    my $client = SM::Client->new;
    my $server = reduce { $a->queue_len <= $b->queue_len ? $a : $b } @servers;
    $server->join_queue($client);
    my $time_of_next = SM::Simulator->now + SM::Client->gen_arrival_interval;
    SM::Simulator->schedule(
        $time_of_next => $handle
    );
};
SM::Simulator->schedule(
    0 => $handle,
);
SM::Simulator->run($count);
