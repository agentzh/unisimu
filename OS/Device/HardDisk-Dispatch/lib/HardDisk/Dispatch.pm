#: HardDisk/Dispatch.pm
#: 2005-11-29 2005-11-29

package HardDisk::Dispatch;

use strict;
use warnings;

use List::MoreUtils qw/first_index/;
use Perl6::Attributes;

our $VERSION = '0.01';

sub new {
	my $proto = shift;
	my $class = ref $proto || $proto;
	my %args = @_;
	my $pos = $args{pos};
	my $self = bless {
		init_pos => $pos,
        init_dir => $args{dir},
		dir => $args{dir},
		plan => [ split(/\s+/, $args{plan}) ],
	}, $class;
    $self->start;
	return bless $self, $class;
}

sub start {
	my $self = shift;
    $.distance = 0;
	$.layout = [$.init_pos, @.plan];
	@.layout = sort { $a <=> $b } @.layout;
	$.init_i = first_index { $_ eq $.init_pos } @.layout;
	$.i = $.init_i;
    $.dir = $.init_dir;
}

sub plan {
    my $self = shift;
    return @.plan;
}

sub pos {
    my $self = shift;
    return $.layout[$.i];
}

sub dir {
    my $self = shift;
    return $.dir;
}

sub distance_moved {
    my $self = shift;
    return $.distance;
}

sub diff {
    my ($self, $i, $j) = @_;
    return $.layout[$i] - $.layout[$j];
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

HardDisk::Dispatch - Perl extension for blah blah blah

=head1 SYNOPSIS

  use HardDisk::Dispatch;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for HardDisk::Dispatch, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.



=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

A. U. Thor, E<lt>a.u.thor@a.galaxy.far.far.awayE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by A. U. Thor

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.


=cut
