package Sim::Clock;

use Carp qw(croak carp);
use strict;
use warnings;

sub new ($$) {
	my $class = ref $_[0] ? ref shift : shift;
	my $now = @_ ? shift : 0;
	bless {
		now => $now,
	}, $class;
}

sub now ($) {
	$_[0]->{now};
}

sub push_to ($$) {
    my ($self, $time) = @_;
	if ($time < $self->now) {
        carp "error: Can't push your time back, sir";
        return 0;
    }
    $self->{now} = $time;
    return 1;
}

sub reset ($) {
    $_[0]->{now} = 0;
}

1;
