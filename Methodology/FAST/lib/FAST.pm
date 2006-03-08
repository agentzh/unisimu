#: FAST.pm
#: Global application class for FAST
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-08

package FAST;

use 5.006001;
use strict;
use warnings;

our $VERSION = '0.01';

sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    return bless {
    }, $class;
}

sub as_png {
}

sub as_asm {
}

sub structured {
}

1;
__END__

=head1 NAME

FAST - Library for Flowchart Abstract Syntax Tree

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang

This library is free software. You can redistribute it and/or
modify it under the same terms as Perl itself.
