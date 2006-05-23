use strict;
use warnings;
use Win32::GuiTest qw(:ALL);
use constant N => 8;
use constant PI => 3.14159265;
use gd::simple;

#sleep 5;
my ($width, $height) = (600, 600);
my $img = GD::Simple->new($width, $height);
$img->bgcolor('white');

$img->fgcolor('red');
#draw_circle(300,300,100);
em_circles(0.5*$width, 0.5*$height,100, 4);

binmode \*STDOUT;
print $img->png;


#
sub em_circles {
	my($ox, $oy, $r, $layers) = @_;
	return if $layers == 0;
	for(0..N-1) {
		my($sox, $soy) = (($ox+2*$r*cos($_*2*PI/N)), ($oy+2*$r*sin($_*2*PI/N)));
		($sox, $soy) = (ceil($sox), ceil($soy));
		my $sr = $r/5;
		my $l = $layers - 1;
		em_circles($sox, $soy, $sr, $l);
	}
	draw_circle($ox, $oy, $r);
}



sub draw_circle {
	my($ox, $oy, $r) = @_;
	$img->moveTo($ox+$r, $oy);
	my $n = 100;
	my $jump = 2*PI/$n;
	for(1..$n) {
		my($x, $y) = (($ox+$r*cos($_*$jump)), ($oy+$r*sin($_*$jump)));
		($x, $y) = (ceil($x), ceil($y));
		$img->lineTo($x, $y);
	}
	
}

sub ceil {
	my $in = shift;
	my $ini = int $in;
	return $ini+1 if($in-$ini >= 0.5);
	return $ini;
}