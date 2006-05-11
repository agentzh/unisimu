#: n_queens.pm
#: 2006-05-11 2006-05-11

package n::Queens;

use strict;
use warnings;

# gen_sols: generates all the solutions of the n-Queens Problem
sub gen_sols {
    my $n = shift;
    my @queens = @_;
    if (@queens == $n) { return @queens ? \@queens : (); }
    my @sols;
    for my $row (0..$n-1) {
        if ( can_put($row, @queens) ) {
            push @sols, gen_sols($n, @queens, $row);
        }
    }
    @sols;
}

# can I put a queen in the specified row of the
#   next coloumn?
sub can_put {
    my $row = shift;
    my @queens = @_;
    my $col = @queens;
    for my $j (0..$#queens) {
        my $i = $queens[$j];
        return undef if $i == $row or
            $i + $j == $row + $col or
            $i - $j == $row - $col;
    }
    1;
}

1;
__END__

=head1 NAME

n_queens - Find all solutions for the n-Queens Problem

=head1 DESCRIPTION

=over

=item The n-Queens Problem

The n-queens problem asks how n queens can be placed on
an n x n chessboard so that no two queens can attack
one another. How can backtracking be used to solve the
n-queens problem?

=item Solution

To solve this problem we must find n positions on an
n x n chessboard so that no two of these positions are
in the same row, same column, or in the same diagonal
[a diagonal consists of all positions (i,j) with i+j=m
for some m, or i-j=m for some m]. We will use backtracking
to solve the n-queens problem. We start with an empty
chessboard. At stage k+1 we attempt putting an additional
queen on the board in the (k+1)st column, where there
are already queens in the first k columns. We examine
squares in the (k+1)st column starting with the square
in the first row, looking for a position to place this
queen so that it is not in the same row or on the same
diagonal as a queen already on the board. (We already
know it is not in the same column.) If it is impossible
to find a position to place the queen in the (k+1)st
column, backtract to the placement of the queen in the
kth column, and place this queen in the next allowable
row in this column, if such a row exists. If no such
row exists, backtract further.

However, we don't backtract manually because the
recursion backtracks automatically for us. Yay!

=back

=head1 SUBROUTINES

=over

=item gen_sols

Generates all the solutions of the n-Queens Problem

=item can_put

Returns a boolean indicating whether a queen can be
put into the specified row of the next coloumn.

=back

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang. All rights reserved.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.
