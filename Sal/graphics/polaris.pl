#expriment 3
#polaris.pl
#2006-5-12

use strict;
use warnings;
use GD::Simple;
use Math::Trig;

#my $angle = shift;
my $gs = (sqrt(5)+0.8) / 2;
my($cx, $cy) = (400, 400);
my $rad = pi / 8;
my $r = 100;
my @points;
my @seeds;						#set seed for every filled area

#generate();
#draw();
#bepaint();
#move(100, 100);
#my $n = 20;
#for(my $i = $n-1; $i >= 0; $i--) {
#	scale($i*1/$n, $i);
#}

for my $i(1 .. 20) {
	#warn $i, "\t";
	move(20*$i, 0,  $i);
	#warn $i, "\n";
}

#for(1..20) {
#	rotate($_*pi/20, $_);
#}
#genpicture($"r/tmp1.png");
#binmode \*STDOUT;
#print $img->png;

sub generate {
	my $img = new GD::Simple(800, 800);
	$img->bgcolor('white');
	my $c1 = $img->colorAllocate(255, 0, 0);
			
	my $c2 = $img->colorAllocate(255, 255, 0);
	my $c3 = $img->colorAllocate(255, 0, 255);
	my $c4 = $img->colorAllocate(0, 255, 255);
	$#points = -1;				#clear array points and seeds
	$#seeds = -1;
	for(0..15) {
		if($_ % 8 == 0) {
			enpoints( $r*$gs, $_, $c1);
		}elsif($_ % 4 == 0) {
			enpoints($r*$gs**3, $_, $c2);
		}elsif($_ % 2 == 0) {
			enpoints($r*$gs**2, $_, $c3);
		}else{
			enpoints($r, $_, $c4);
		}
	}
	#draw();
	return $img;
}

sub draw {
	my $img = shift;
	for(0..15) {
		$img->moveTo($cx, $cy);
		$img->lineTo($points[$_*2], $points[$_*2+1]);
	}
	for(0..15) {
		$img->lineTo($points[(14-$_)*2], $points[(14-$_)*2+1]);
	}
}
sub bepaint {
	my $img = shift;
	for(0..15) {
		$img->fill($seeds[3*$_], $seeds[3*$_+1], $seeds[3*$_+2]);
	}
}
	

sub enpoints {
	my($r, $i, $c) = @_;
	push @points, $cx+$r*cos($i*$rad), $cy+$r*sin($i*$rad);
	push @seeds, $cx+0.125*$r*cos($i*$rad+0.5*$rad), $cy+0.125*$r*sin($i*$rad+0.5*$rad), $c;
}


sub move {
	my($x, $y, $s) = @_;
	$cx += $x;
	$cy += $y;
	warn $s, "\n";

	my $img = generate();
	draw($img);
	bepaint($img);
	genpicture($img, "m/tmp$s.png");
}

sub scale {
	my $sc = shift;
	my $s = shift || '';
	$r *= $sc;
	my $img = generate();
	draw($img);
	bepaint($img);
	genpicture($img, "s/tmp$s.png");
}

sub rotate {
	my ($a, $s) = @_;
	my $img = generate();
	for(0..15) {
		my($x, $y, $sx, $sy) = ($points[$_*2], $points[$_*2+1], $seeds[$_*3], $seeds[$_*3+1]);
		$points[$_*2] = $cx + ($x-$cx)*cos($a) - ($y-$cy)*sin($a);
		$points[$_*2+1] = $cy + ($y-$cy)*cos($a) + ($x-$cx)*sin($a);
		$seeds[$_*3] = $cx + ($sx-$cx)*cos($a) - ($sy-$cy)*sin($a);
		$seeds[$_*3+1] = $cy + ($sy-$cy)*cos($a) + ($sx-$cx)*sin($a);
	}
	draw($img);
	bepaint($img);
	genpicture($img, "r/tmp$s.png");
}

sub genpicture {
	my ($img, $fn) = @_;
	open my $png, ">$fn" or
		die "cannot open $fn:$!\n";
	binmode $png;
	print $png $img->png;
	close $png or die "$!\n";
}
