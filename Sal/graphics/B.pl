use strict;
use warnings;
use GD::Simple;
use constant N => 100;
use constant PI => 3.1415926535;

my $i = 0;
my $img = GD::Simple->new(800, 800);

$img->bgcolor('white');
$img->fgcolor('black');

my $ver_no = 10;#4 + int rand 6;
my $step = 1/N;
my (@points, @cofx, @cofy);

my $r = 100;
my($x, $y) = (300, 300);
my $rad = 0.2 * PI;
for(1..$ver_no) {
	$_ -= 2*$_ if int rand 2;
	$x += int $r*cos($_ * $rad);
	$y += int $r*sin($_ * $rad);
	push @points, $x, $y;
	warn $x, " === ", $y, "\n";
}

#@points = (300, 300, 350, 387, 300,  473, 200, 473, 150, 387, 200, 300, 300, 300);
my @groups;

for my $g(0..@points/2-4) {
	
	my @ge;
	
	for(0..7) {
		push @ge, $points[2*$g+$_];
	}
	push @groups, \@ge;
}

for(@groups) {
	warn join "\t", @$_, "  here\n";
	$i = 0;
	#my $gg = [50, 100, 160, 180, 220, 120, 350, 50];
	draw($_);
}

binmode \*STDOUT;
print $img->png;


sub draw {
	my $g = shift;
	init($g);
	
	$img->moveTo($g->[0], $g->[1]);
	$img->lineTo($g->[2], $g->[3]);
	$img->lineTo($g->[4], $g->[5]);
	$img->lineTo($g->[6], $g->[7]);
	$img->moveTo($g->[0], $g->[1]);

	for(1..N) {
		my $t = $_ * $step;
		line($t);
	}
}

sub line {
	my $t = shift;
	my($x, $y) = (0, 0);
	
	#warn join "\t", @cofx, "\n";
	#warn join "\t", @cofy, "\n";
	for(my $i = 3; $i >= 0; $i--) {
		$x += $cofx[$i]*$t**$i;
		$y += $cofy[$i]*$t**$i
	}
	#warn "$t ==> $x :  $y\n";
	if ($i == 0) {
		$img->moveTo($x, $y) ;
		$i++;
	}
	$img->lineTo($x, $y) unless $i == 0;
}

sub init {
	my $g = shift;
	$#cofx = -1;
	
	$#cofy = -1;
	

	my($x0, $x1, $x2, $x3) = ($g->[0], $g->[2], $g->[4], $g->[6]);
	my($y0, $y1, $y2, $y3) = ($g->[1], $g->[3], $g->[5], $g->[7]);

	push @cofx, $x0 + 4*$x1 + $x2,
				-3*$x0 + 3* $x2,
				3*$x0 - 6*$x1 + 3*$x2,
				-$x0 + 3*$x1 - 3*$x2 + $x3;
	
	push @cofy, $y0 + 4*$y1 + $y2,
				-3*$y0 + 3* $y2,
				3*$y0 - 6*$y1 + 3*$y2,
				-$y0 + 3*$y1 - 3*$y2 + $y3;

	map {$_ /= 6} @cofx;
	map {$_ /= 6} @cofy;
	#warn join "\t", @cofx, " !\n";
	#warn join "\t", @cofy, " !\n";

}






