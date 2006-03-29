package drawgraph2;
require Exporter;
@ISA = qw/Exporter/;
@EXPORT = qw/gen_img animate synerror substring/;


use strict;
use warnings;
use Math::Trig;
use GD::Simple;
use Win32::GuiTest qw(:ALL);

my $n = 1000;



sub animate {
	my($fh, $t) = @_;
	#die %$fh;

	system "start /max mspaint";
	#my @paint =WaitWindowLike(undef, "画图");

	sleep(1);
	warn "i can find painting window!\n";	
	#SetForegroundWindow($paint[0]);	
	#die;
	SendKeys("%ia");
	sleep(1);
	SendKeys("1024{TAB}768");
	SendKeys("~");
	sleep(1);	
	for my $i (1..$n) {
		#die;
		my($x, $y) = get_point($fh, $t, $i);
		
		(MouseMoveAbsPix($x, $y), SendLButtonDown()) if $i == 1;	
		MouseMoveAbsPix($x, $y);
	}
	SendLButtonUp();
	SendKeys("%fa");	
	sleep(1);
	
	my @mt = localtime();
	$mt[5] += 1900;
	$mt[4] += 1;
	my $name;
	for(0..5) {
		$name .= $mt[5-$_];
	}

	SendKeys("D:\\zwx\\$name.jpg");
	SendKeys("~");
	SendKeys("{PAU 1000}%{F4}");
	
}	

sub gen_img {
	my($fh, $t) = @_;
	my $im = GD::Simple->new(400,400);
	my $red = $im->colorAllocate(255,0,0);
	
	for my $i (1..$n) {
		my($x, $y) = get_point($fh, $t, $i);
		$im->moveTo($x, $y) if $i == 1;	
		$im->lineTo($x, $y);
	}
	binmode \*STDOUT;
	print $im->png;
}
sub get_point {
	my ($exp, $t, $i) = @_;	
	my $trange;
	if($t == 1) {
		$trange = 1;
	} else {
		$trange = 4*pi;
	}
	my $step = $trange / $n;
	my $s = $i * $step;
	#print $s, "\n";
	my($x, $y);
	if ($t == 1) {
		($x, $y) = (200 + getvalue($exp->{'x'}, $s), 200 + getvalue($exp->{'y'}, $s));	
		#print $x, "=> ", $y, "\n";
		#warn "here 1\n";
		
	} elsif($t == 2) {
		($x, $y) = (200 + getvalue($exp->{'x'}, $s), 200 + getvalue($exp->{'y'}, $s));
		#warn "here 2\n";
	} else {
		
		($x, $y) = (200 + getvalue($exp->{'r'}, $s) * cos($s), 200 + getvalue($exp->{'r'}, $s) * sin($s));
		#warn $x, "=> ", $y, "\n";		
		#warn "here 3\n";
	}
	return ($x, $y);	
}

sub getvalue {
	my ($expression, $step) = @_;
	#warn $expression, "\n";
	
	#print $step;
	$expression =~ s/t/$step/g;
	#print $expression;
	return eval ($expression);
}

sub synerror {
	 my $ln = shift;
	 die "syntax error at line $ln\n";
 }

 sub substring {
	 my($h1, $arg) = @_;
	 while (my($KEY, $VALUE) = each %$h1) {
		 #print $VALUE, "\n";
		 while(my($key, $value) = each %$arg) {
			 $VALUE =~ s/$key/$value/; 			 
		 }
		if($VALUE =~ /t/) {
			$h1->{$KEY} = $VALUE;
		} else {
			$h1->{$KEY} = eval($VALUE);
		}
	 }
 }
 

#animate("ellipse.txt", 2);

1;

