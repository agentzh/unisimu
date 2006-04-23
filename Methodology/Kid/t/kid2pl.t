#: kid2pl.t
#: test ../script/kid2pl.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-23 2006-04-23

use strict;
use warnings;
use Test::More tests => 4;
use File::Compare 'compare_text';

unlink "t/01test.pl" if -f "t/01test.pl";
is system("$^X -Ilib script/kid2pl.pl t/01test.kid"), 0;
is compare_text("t/01test.pl", "t/~01test.pl"), 0;

unlink "t/02test.pl" if -f "t/02test.pl";
is system("$^X -Ilib script/kid2pl.pl t/02test.kid"), 0;
is compare_text("t/02test.pl", "t/~02test.pl"), 0;
