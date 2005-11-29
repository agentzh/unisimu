#: HardDisk/Dispatch/FCFS.pm
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-29 2005-11-29

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
    $.dir = $.init_dir;
}

sub move_next {
	my $self = shift;
	if ($.i + 1 < @.layout) {
        my $d = $self->diff($.i+1, $.i);
        $.dir = $d > 0 ? '+' : '-';
        $.distance += abs $d;
        return $.layout[++$.i];
	} else {
        return undef;
    }
}

1;
__END__
