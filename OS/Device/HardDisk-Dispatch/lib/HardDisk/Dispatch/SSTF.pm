#: HardDisk/Dispatch/SSTF.pm
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-29 2005-11-29

package HardDisk::Dispatch::SSTF;

use strict;
use warnings;
use base 'HardDisk::Dispatch';
use Perl6::Attributes;

our $VERSION = '0.01';

sub move_next {
	my $self = shift;
	if ($.i == 0) {
        $.distance = $.layout[$.i+1] - $
        return $.layout[++$.i];
	} elsif ($.i < @.layout) {
        my $diff1 = $.layout[$.i-1] < $.layout[$.i]) {

        return $.plan[$.i++];
    } else {
        return undef;
    }
}

sub diff {
    my ($self, $i, $j) = @_;
    return abs($.layout[$j] - $.layout[$i]);
}

1;
__END__
