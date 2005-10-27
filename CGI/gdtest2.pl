use strict;
use warnings;

use GD::Simple;
use GD::Stack;

my $stack = GD::Stack->new(100,200,450);

assign(1,0,5);
assign(2,5,10);
assign(3,25,5);

#$stack->cyclic(5,10);
#$stack->cyclic(5,20);

binmode \*STDOUT;
print $stack->as_png;

sub assign {
    $stack->draw_used_block(@_);
}