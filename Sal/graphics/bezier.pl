use strict;
use warnings;
use GD::Simple;
use constant N => 100;

my $img = GD::Simple->new(500, 500);

$img->bgcolor('white');
$img->fgcolor('red');

my $ver_no = 4; # + int rand 6;
my $step = 1/N;
my (@points, @cofx, @cofy);
for(1..$ver_no) {
	my($x, $y) = (int rand 300, int rand 300);
	push @points, $x, $y;
}


my $gg = \@points;
draw($gg);


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
	$img->lineTo($x, $y);
}

sub init {
	my $g = shift;
	$#cofx = -1;
	
	$#cofy = -1;
	

	my($x0, $x1, $x2, $x3) = ($g->[0], $g->[2], $g->[4], $g->[6]);
	my($y0, $y1, $y2, $y3) = ($g->[1], $g->[3], $g->[5], $g->[7]);

	push @cofx, $x0,
				-3*$x0 + 3*$x1,
				3*$x0 -6*$x1 + 3*$x2,
				 -$x0 + 3*$x1 - 3*$x2 + $x3
		;
	
	push @cofy, $y0,
				-3*$y0 + 3*$y1,
				3*$y0 -6*$y1 + 3*$y2,
				 -$y0 + 3*$y1 - 3*$y2 + $y3
		;

	
	
}






