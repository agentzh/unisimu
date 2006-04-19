#: dda.pl
#: Digital differential analyzer (DDA) routine for
#:   rasterizing a line
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-10 2006-04-19

use strict;
use warnings;
use GD::Simple;

my $im = GD::Simple->new(40, 50);
my $red = $im->colorAllocate(255, 0, 0);

draw_line($im, 0, 0, 20, 30);

binmode \*STDOUT;
print $im->png;

sub draw_line {
    my ($im, $x1, $y1, $x2, $y2) = @_;
    # approximate the line length
    my $Length;
    if (abs($x2 - $x1) >= abs($y2 - $y1)) {
        $Length = abs($x2 - $x1);
    } else {
        $Length = abs($y2 - $y1);
    }
    # select the larger of delta_x or delta_y to be one raster unit
    my $delta_x = ($x2 - $x1) / $Length;
    my $delta_y = ($y2 - $y1) / $Length;
    # round the values rather than truncate, so that center
    # pixel addressing is handled correctly
    my $x = $x1 + 0.5;
    my $y = $y1 + 0.5;
    # begin main loop
    my $i = 0;
    while ($i <= $Length) {
        $im->setPixel(int($x), int($y), $red);
        $x += $delta_x;
        $y += $delta_y;
        $i++;
    }
}
