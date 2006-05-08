# string.t

use strict;
use warnings;
use File::Compare;

use Test::More tests => 6;

BEGIN { use_ok('GD::Animation'); }

my $anim = GD::Animation->new(
    200, 250,
    loops => 0,
    delay => 10,
    colors => {
        white => [255, 255, 255],
        red   => [255, 0, 0],
    },
);
ok $anim;
isa_ok $anim, 'GD::Animation';

my $x = 50;
for (1..6) {
    my $im = $anim->add_frame;
    my %colors = $anim->color_map;
    $im->bgcolor( $colors{white} );
    $im->fgcolor( $colors{red}   );
    $im->lineTo( $x, 190 );
    $x += 10;
}

$x = 50;
my $im;
for (1..6) {
    $im = $anim->add_frame($im);
    my %colors = $anim->color_map;
    $im->fgcolor( $colors{red} );
    $im->bgcolor( $colors{white} );
    $im->lineTo( $x, 190 );
    $x += 10;
}

my $out;
ok open($out, "> t/string.gif"), 'file creation ok';
binmode $out;
print $out $anim->gif_data;
ok close $out;
#warn "ok";

is File::Compare::compare('t/string.gif', 't/~string.gif'), 0;
