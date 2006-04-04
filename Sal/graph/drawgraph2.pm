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
my ($off_x, $off_y);

sub animate {
	($off_x, $off_y) = (0, 0);
	my($fh, $t) = @_;
	#die %$fh;

	#print "here";
	my @paint = FindWindowLike(0, "»­Í¼", "");
	#SetForegroundWindow($paint[0]) if ;
	#die @paint if not @paint;
	
	unless(@paint) {
		print "new palette will start!\n";
		system "start /max mspaint";
	    @paint = WaitWindowLike(0, "»­Í¼", "");

	 
		warn "i can find painting window!\n";	
		SetForegroundWindow($paint[0]);	
		sleep 2;
		#die;
		SendKeys("%ia");
		sleep(1);
		SendKeys("1024{TAB}768");
		SendKeys("~");
		sleep(1);
	}	
	for my $i (1..$n) {
		#die;
		SetForegroundWindow($paint[0]);	
		my($x, $y) = get_point($fh, $t, $i);
		if ($i == 1) {
			$off_x = 500 - $x;
			$off_y = 380 - $y;
			next;
			
		} 
		if($i == 2) {
			MouseMoveAbsPix($x, $y);
			SendLButtonDown();	
			print $x, "=> ", $y, "\n";
			next;
		}
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

	SendKeys("E:\\samples\\$name.jpg");
	SendKeys("%s");
	sleep 4;
	SendKeys("%fn");
	sleep 4;		
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
		($x, $y) = ($off_x + getvalue($exp->{'x'}, $s), $off_y + getvalue($exp->{'y'}, $s));	
		#print $x, "=> ", $y, "\n";
		#warn "here 1\n";
		
	} elsif($t == 2) {
		($x, $y) = ($off_x + getvalue($exp->{'x'}, $s), $off_y + getvalue($exp->{'y'}, $s));
		#warn "here 2\n";
	} else {
		
		($x, $y) = ($off_x + getvalue($exp->{'r'}, $s) * cos($s), $off_y + getvalue($exp->{'r'}, $s) * sin($s));
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
	my $res = eval ($expression);
	return $res unless $@;
	#die;
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
