use strict;
use warnings;
use vorp_par;
use par_vp;
use Win32::GuiTest qw(:ALL);
use threads;


my $th1 = threads->new(\&break);
my $th2 = threads->new(\&draw);

my $s = $th1->join;
$th2->detach();

sub draw {
	my $str = shift || "syn/*.syn";
	my @files = glob($str);
	for(@files) {
		exit (0) if $s;
		if(/vec_par/) {
			anvp ($_ , 1);
		} elsif(/vect/) {
			anvorp($_ , 2);
		} elsif(/polar/) {
			anvorp ($_ , 3);
		} else {
			die "no syntax file found!\n";
		}
	}
}
sub break {
	while(1) {
		if(IsKeyPressed("ESC")) {
			print "drawing will be terminated!\n";
			last;
		}
		
	}
	return 1;
}