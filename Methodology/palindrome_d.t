#: palindrome_d.t
#: Test the executable built from palindrome.d
#:   using palindrome.t
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-04 2006-03-04

use strict;
use warnings;
use FindBin;

system("$^X palindrome.t palindrome.exe");
