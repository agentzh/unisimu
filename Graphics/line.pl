#: line.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-17 2006-03-17

use strict;
use warnings;
use GD::Simple;

my $img = GD::Simple->new(40,50);
$img->bgcolor('white');
$img->fgcolor('red');

$img->moveTo(0, 0);
$img->lineTo(20, 30);

binmode \*STDOUT;
print $img->png;
