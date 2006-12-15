package SM::Simulator;

use strict;
use warnings;
use base 'Exporter';
use base 'Sim::Dispatcher';

our @EXPORT_OK = qw(log);

sub log {
    my $now = __PACKAGE__->now;
    warn "\@$now @_\n";
}

1;
