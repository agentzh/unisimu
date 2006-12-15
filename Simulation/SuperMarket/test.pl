#!/usr/bin/perl

use strict;
use warnings;
BEGIN { push @INC, 'lib'; }
use SM;

my $count = shift || 10;
my $server = SM::Server->new;
my $handle;
$handle = sub {
    my $client = SM::Client->new;
    $server->join_queue($client);
    my $time_of_next = SM::Simulator->now + SM::Client->gen_time_interval;
    SM::Simulator->schedule(
        $time_of_next => $handle
    );
};
SM::Simulator->schedule(
    0 => $handle,
);
SM::Simulator->run($count);
