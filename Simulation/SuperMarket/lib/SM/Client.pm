package SM::Client;

use strict;
use warnings;
use SM::Simulator;
use overload '""' => sub { $_[0]->id };

our $ArrivalInterval = sub { 1 };

my $Counter;

sub new {
    my $class = ref $_[0] ? ref shift : shift;
    my $self = bless {
        id => $Counter++,
        interval => @_ ? shift : 1,
    }, $class;
}

sub id {
    $_[0]->{id};
}

sub gen_arrival_interval {
    $ArrivalInterval->();
}

1;
