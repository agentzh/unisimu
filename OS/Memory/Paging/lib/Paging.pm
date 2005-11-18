#: Paging.pm
#: Copyright (c) Agent Zhang
#: 2005-11-17 2005-11-18

package Paging;

use strict;
use warnings;

our $VERSION = '0.01';

package Paging;

sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    my %opts = @_;
    $opts{mem_size} ||= 4;
    my $self = {
        _queue    => [],
        _mem_size => $opts{mem_size},
    };
    return bless $self, $class;
}

sub queue {
    my $self = shift;
    return @{$self->{_queue}};
}

sub mem_size {
    return shift->{_mem_size};
}

1;
__END__

=head1 NAME

Paging - Perl simulation for OS paging technology

=head1 SYNOPSIS

  use Paging::FIFO;
  $fifo = Paging::FIFO->new(mem_size => 4);
  $miss = 0;
  for (qw(1 2 3 4 5 6)) {
      if (not $fifo->access($_)) {
          $miss++;
          $old_page = $fifo->alloc($_);
          if ($old_page) {
              warn "  Replacing page $old_page with $_";
          }
      }
  }
  warn "For total $miss misses";

  use Paging::LRU;
  $lru  = Paging::LRU->new;
  $miss = 0;
  for (qw(1 2 3 4 5 6)) {
      if (not $lru->access($_)) {
          $miss++;
          $old_page = $lru->alloc($_);
          if ($old_page) {
              warn "  Replacing page $old_page with $_";
          }
      }
  }
  warn "For total $miss misses";

=head1 DESCRIPTION

This library provides the implementation for two paging dispatch algorithms, 
FIFO (First-In-Firt-Out) and LRU (Least-Recently-Used).

=head1 CLASSES

Paging is simply a base class, you shouldn't use it directly unless you are
implementing a third algorithm for paging. In the latter situation, it is
recommended to inherit your class from the Paging class.

In this library, I provides two subclasses for Paging, say, Paging::FIFO and
Paging::LRU. They implement the actual functionalities. Please consult their
documents L<Paging::FIFO> and L<Paging::LRU> respectively.

=head2 EXPORT

None by default.

=head1 SEE ALSO

L<Paging::FIFO>, L<Paging::LRU>

=head1 AUTHOR

Agent Zhang, E<lt>agent2002@126.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 Agent Zhang

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
