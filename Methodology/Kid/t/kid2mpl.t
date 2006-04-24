#: kid2mpl.t
#: test ../script/kid2mpl.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-23 2006-04-23

use strict;
use warnings;
use Test::More tests => 4;
use File::Compare 'compare_text';

unlink "t/01test.mpl" if -f "t/01test.mpl";
is system("$^X -Ilib script/kid2mpl.pl t/01test.kid"), 0;
is compare_text("t/01test.mpl", "t/~01test.mpl"), 0;

unlink "t/02test.mpl" if -f "t/02test.mpl";
is system("$^X -Ilib script/kid2mpl.pl t/02test.kid"), 0;
is compare_text("t/02test.mpl", "t/~02test.mpl"), 0;
