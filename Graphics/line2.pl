#: line2.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-17 2006-03-17

use strict;
use warnings;
use GD::Simple;

my $im = GD::Simple->new(40,50);
my $red = $im->colorAllocate(255,0,0); 

#$img->moveTo(0, 0);
#$img->lineTo(20, 30);
draw_line($im, 0, 0, 20, 30);

binmode \*STDOUT;
print $im->png;

sub draw_line {
    my ($im, $x_O, $y_O, $x_A, $y_A) = @_;
    my ($x, $y) = ($x_O, $y_O);
    $im->setPixel($x, $y, $red);
    while ($x != $x_A || $y != $y_A) {
        my $F_M = $y * $x_A - $y_A * $x;
        if ($F_M < 0) {
            $im->setPixel($x, ++$y, $red);
        } elsif ($F_M > 0) {
            $im->setPixel(++$x, $y, $red);
        } else {
            $im->setPixel(++$x, ++$y, $red);
        }
    }
    #$im->setPixel($x, $y, $red);
}
