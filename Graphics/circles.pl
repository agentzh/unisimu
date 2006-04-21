#: circles.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-10 2006-04-21

use strict;
use warnings;
use GD::Simple;
use constant {
	PI => 3.1415926,
	MAX_DEPTH => 5,
};

# $R_scale 表示大圆的半径 $R 与大小圆圆心径 $D 的比例
# $D_scale 表示相邻上下两层的圆心距的比例
my ($R_scale, $D_scale) = (0.5, 0.2);
my $nelems = 10;
my ($width, $height) = (600, 600);
my $img = GD::Simple->new($width, $height);
$img->bgcolor('white');
$img->fgcolor('red');

draw_circles($img, 0.5*$width, 0.5*$height, 0.33 * $width);
binmode \*STDOUT;
print $img->png;

sub draw_circles {
    # $R 表示大圆的半径，$D 表示大小圆之间的圆心距
    my ($img, $x0, $y0, $D, $depth) = @_;
	$depth ||= 1;
	return if $depth == MAX_DEPTH;
	my $R = $D * $R_scale;
	$img->moveTo($x0, $y0);
	$img->ellipse(2 * $R, 2 * $R);
	my $delta_angle = 2 * PI / $nelems;
	for my $i (1..$nelems) {
		my $angle = $delta_angle * $i;
		my $x = $D * cos($angle) + $x0;
		my $y = $D * sin($angle) + $y0;
		draw_circles($img, $x, $y, $D * $D_scale, $depth+1);
	}
}
