#: HardDisk/Dispatch/SSTF.pm
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-29 2005-11-29

package HardDisk::Dispatch::SSTF;

use strict;
use warnings;
use base 'HardDisk::Dispatch';
use Perl6::Attributes;
#use Smart::Comments;

our $VERSION = '0.01';

sub move_next {
	my $self = shift;
    ### $.i
    return if not @.layout or @.layout == 1;
	if ($.i == 0) {
        $.i++;
        my $d = $self->diff($.i, $.i-1);
        shift @.layout;
        $.distance += abs $d;
        $.dir = $d > 0 ? '+' : '-';
        return $.layout[$.i];
    } elsif ($.i == @.layout - 1) {
        $.i--;
        my $d = $self->diff($.i, $.i+1);
        pop @.layout;
        $.distance += abs $d;
        $.dir = $d > 0 ? '+' : '-';
        return $.layout[$.i];
    } elsif ($.i < @.layout) {
        my $left_d  = $self->diff($.i-1, $.i);
        my $right_d = $self->diff($.i+1, $.i);
        if (abs $left_d < abs $right_d) {
            splice @.layout, $.i, 1;
            $.i--;
            $.dir = $left_d > 0 ? '+' : '-';
            $.distance += abs $left_d;
            return $.layout[$.i];
        #} elsif (abs $left_d == abs $right_d) {
            #
        } else { # $left_d > $right_d
            splice @.layout, $.i, 1;
            $.dir = $right_d > 0 ? '+' : '-';
            $.distance += abs $right_d;
            return $.layout[$.i];
        }
    } else {
        return undef;
    }
}

1;
__END__
