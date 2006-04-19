#: bresenham.pl
#: Generalized integer Bresenham's line rasterization algorithm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-19 2006-04-19

use strict;
use warnings;
use GD::Simple;

my $im = GD::Simple->new(40, 50);
my $red = $im->colorAllocate(255, 0, 0);

draw_line($im, 0, 0, 20, 30, $red);

binmode \*STDOUT;
print $im->png;

sub draw_line {
    my ($im, $x1, $y1, $x2, $y2, $color) = @_;
    my $x = $x1;
    my $y = $y1;
    my $delta_x = abs($x2 - $x1);
    my $delta_y = abs($y2 - $y1);
    my $s1 = sign($x2 - $x1);
    my $s2 = sign($y2 - $y1);
    # interchange delta_x and delta_y, depending on the slope
    #   of the line
    my $Interchange;
    if ($delta_y > $delta_x) {
        ($delta_x, $delta_y) = ($delta_y, $delta_x);
        $Interchange = 1;
    }
    # initialize the error term to compensate for a nonzero intercept
    my $e = 2 * $delta_y - $delta_x;
    # begin the main loop
    for my $i (0..$delta_x) {
        $im->setPixel($x, $y, $red);
        while ($e > 0) {
            if ($Interchange) {
                $x += $s1;
            } else {
                $y += $s2;
            }
            $e -= 2 * $delta_x;
        }
        if ($Interchange) {
            $y += $s2;
        } else {
            $x += $s1;
        }
        $e += 2 * $delta_y;
    }
}

sub sign {
    return $_[0] <=> 0;
}
