#: HardDisk/Dispatch/C_SCAN.pm
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-29 2005-11-29

package HardDisk::Dispatch::C_SCAN;

use strict;
use warnings;

use base 'HardDisk::Dispatch';
use Perl6::Attributes;
#use Smart::Comments;

our $VERSION = '0.01';

sub move_next {
	my $self = shift;
    #my $i = $.i;
    ### $i
    #my $s = join(' ', @.layout);
    ### $s
    return if not @.layout or @.layout == 1;
	if ($.i == 0) {
        if ($.init_dir eq '-') {
            $.i = @.layout - 1;
            my $d = $self->diff($.i, 0);
            shift @.layout;
            $.i--;
            $.distance += $d;
            $.dir = '+';
            return $.layout[$.i];
        }
        my $d = $self->diff(1, 0);
        shift @.layout;
        $.distance += $d;
        $.dir = '+';
        return $.layout[0];
    } elsif ($.i == @.layout - 1) {
        if ($.init_dir eq '+') {
            my $d = $self->diff(0, $.i);
            pop @.layout;
            $.distance += -$d;
            $.dir = '-';
            $.i = 0;
            return $.layout[0];
        }
        $.i--;
        my $d = $self->diff($.i, $.i+1);
        pop @.layout;
        $.distance += -$d;
        $.dir = '-';
        return $.layout[$.i];
    } elsif ($.i < @.layout) {
        if ($.init_dir eq '-') {
            my $d = $self->diff($.i-1, $.i);
            splice @.layout, $.i, 1;
            $.distance += -$d;
            $.dir = '-';
            $.i--;
        } else {
            my $d = $self->diff($.i+1, $.i);
            splice @.layout, $.i, 1;
            $.distance += $d;
            $.dir = '+';
        }
        return $.layout[$.i];
    } else {
        return undef;
    }
}

1;
__END__
