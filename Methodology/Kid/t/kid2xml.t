#: kid2xml.t
#: test ../script/kid2xml.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-23 2006-04-23

use strict;
use warnings;
use Test::More tests => 4;
use File::Compare 'compare_text';

unlink "t/01test.xml" if -f "t/01test.xml";
is system("$^X -Ilib script/kid2xml.pl t/01test.kid"), 0;
is compare_text("t/01test.xml", "t/~01test.xml"), 0;

unlink "t/02test.xml" if -f "t/02test.xml";
is system("$^X -Ilib script/kid2xml.pl t/02test.kid"), 0;
is compare_text("t/02test.xml", "t/~02test.xml"), 0;
