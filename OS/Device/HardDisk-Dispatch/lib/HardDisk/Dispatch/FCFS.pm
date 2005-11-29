package HardDisk::Dispatch::FCFS;

use strict;
use warnings;
use base 'HardDisk::Dispatch';
use Perl6::Attributes;

our $VERSION = '0.01';

sub start {
	my $self = shift;
    $.distance = 0;
	$.layout = [$.init_pos, @.plan];
	$.init_i = 0;
	$.i = $.init_i;
}

sub move_next {
	my $self = shift;
	if ($.i < @.plan) {
        return $.plan[$.i++];
	} else {
        return undef;
    }
}

1;
__END__
