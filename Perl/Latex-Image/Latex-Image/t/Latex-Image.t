use strict;
use warnings;
use File::Compare;

use Test::More tests => 5;
BEGIN { use_ok('Latex::Image') };

my $DEBUG = 0;

my $im1 = 't/foo.png';
my $im2 = 't/equ.jpg';

ok(Latex::Image->convert(
  'So we get $\sqrt {a^2  + b^2 }$.',
  $im1,
  density => '73x73',
));

is File::Compare::compare($im1, 't/~foo.png'), 0;

my $latex = <<'_EOC_';
\[
  \frac{{n!}}
  {{r!\left( {n - r} \right)!}}
\]
_EOC_
ok(Latex::Image->convert(
    $latex,
    $im2,
    border => 20,
    density => '200x200'));

is File::Compare::compare($im2, 't/~equ.jpg'), 0;

END {
    if (!$DEBUG) {
        unlink $im1 if -f $im1;
        unlink $im2 if -f $im2;
    }
}
