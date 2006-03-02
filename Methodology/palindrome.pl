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

__END__

一字符串若从正反两个方向读是相同的，称为回文。若不计空格从正、反两个方向
读是相同的，称为组合回文。设计一程序，判断一输入字符串是回文、组合回文或
者不是回文。
