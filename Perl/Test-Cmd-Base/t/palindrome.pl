#: palindrome.pl
#: Problem 5, Chapter 1
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-27 2006-02-27

use strict;
use warnings;

sub check { reverse eq $_ }
$_ = <>; chomp;
if (check) { print "palindrome\n" }
elsif (s/\s+//g and check) { print "combinational palindrome\n" }
else { print "not palindrome\n" }
