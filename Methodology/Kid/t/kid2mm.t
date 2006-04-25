#: kid2mm.t
#: test ../script/kid2mm.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-26 2006-04-26

use strict;
use warnings;
use Test::More tests => 6;
use File::Compare 'compare_text';

unlink "t/01test.mm" if -f "t/01test.mm";
is system("$^X -Ilib script/kid2mm.pl t/01test.kid"), 0;
ok -f "t/01test.mm";
is compare_text("t/01test.mm", "t/~01test.mm"), 0;

unlink "t/02test.mm" if -f "t/02test.mm";
is system("$^X -Ilib script/kid2mm.pl t/02test.kid"), 0;
ok -f "t/02test.mm";
is compare_text("t/02test.mm", "t/~02test.mm"), 0;
