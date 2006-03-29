use strict;
use warnings;

my $str = shift || "syn/*.syn";
my @files = glob($str);
for(@files) {
	if(/vec_par/) {
		system "perl par_vp.pl $_ 1";
	} elsif(/vect/) {
		system "perl vorp_par.pl $_ 2";
	} elsif(/polar/) {
		system "perl vorp_par.pl $_ 3";
	} else {
		die "no syntax file found!\n";
	}
}