#: pocker.pl
#: 《概率论》作业 Page 60, 习题5
#: 随机试验法
#：zx_agent. All rights reserved.
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-28 2005-04-01

use strict;
use warnings;
use Math::Random;

my $nexps = shift;
$nexps = 100_000 unless defined $nexps;

my @sorts;
my @freqs;

my $i = 0;
foreach (1..12) {$sorts[$i++] = 'spade'; }
foreach (1..12) {$sorts[$i++] = 'club'; }
foreach (1..12) {$sorts[$i++] = 'heart'; }
foreach (1..12) {$sorts[$i++] = 'diamond'; }

map{
	$_ = 0;
} @freqs;

foreach  (1..$nexps) {
	my @perm = random_permutation(@sorts);
	@perm = splice(@perm, 0, 5);
	my $spadecount = 0;
	map{
		++$spadecount if $_ eq 'spade';
	} @perm;
	++$freqs[$spadecount];
}

print "The probability of drawing no spades is ",
      trim($freqs[0]/$nexps)," (",
	  trim(C(39, 5) / C(52, 5))," expected).\n"; 

print "The probability of drawing one spade is ",
      trim($freqs[1]/$nexps)," (",
	  trim(C(39, 4) * C(13, 1) / C(52, 5))," expected).\n"; 

print "The probability of drawing two spades is ",
      trim($freqs[2]/$nexps)," (",
	  trim(C(39, 3) * C(13, 2) / C(52, 5))," expected).\n"; 

print "The probability of drawing three spades is ",
      trim($freqs[3]/$nexps)," (",
	  trim(C(39, 2) * C(13, 3) / C(52, 5))," expected).\n"; 

print "The probability of drawing four spades is ",
      trim($freqs[4]/$nexps)," (",
	  trim(C(39, 1) * C(13, 4) / C(52, 5))," expected).\n";

print "The probability of drawing five spades is ",
      trim($freqs[5]/$nexps)," (",
	  trim(C(13, 5) / C(52, 5))," expected).\n"; 

sub C {
	my ($n, $m) = @_;
	return fac($n) / (fac($m) *fac($n - $m));
}

sub fac {
	my $n = shift;
	return 1 if $n == 0;
	my $res = 1;
	foreach my $i (2..$n) {
		$res *= $i;
	}
	return $res;
}

sub trim {
	return sprintf("%.4f", $_[0]);
}

__END__

从一副扑克牌中（52 张）任取出 5 张牌，求其中黑桃张数的分布律。
