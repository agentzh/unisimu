package SM::Client;

use strict;
use warnings;
use SM::Simulator;
use overload '""' => sub { $_[0]->id };

my $Counter;

sub new {
    my $class = ref $_[0] ? ref shift : shift;
    my $self = bless {
        id => $Counter++,
    }, $class;
}

sub id {
    $_[0]->{id};
}

sub gen_time_interval {
    1;
}

1;
