# simple.t

use strict;
use warnings;
use File::Compare;
use Test::More tests => 27;
#use Data::Dumper::Simple;

BEGIN { use_ok('GD::Animation'); }

my $anim = GD::Animation->new(
    200, 250,
    colors => {
        white => [255, 255, 255],
        red   => [255, 0, 0],
        green => [0, 255, 0],
        blue  => [0, 0, 255],
    },
);
ok $anim, 'obj ok';
isa_ok $anim, 'GD::Animation';

my $gif = $anim->gif_data;
ok !defined $gif, 'gif data not defined';

#warn "???";
my $im = $anim->add_frame;
#warn "!!!";

my %colors = $anim->color_map;
#warn Dumper(%colors);
my @keys = sort keys %colors;
is join(' ', @keys), 'blue green red white', 'color map keys ok';
my @values = sort values %colors;
#warn Dumper(@values);
like join(' ', @values), qr/[ \d]+/, 'color map value ok';

$im->bgcolor($colors{white});
$im->fgcolor($colors{red});

my $d = 50;
#$im->ellipse(50, 50, $d, $d, $colors{green});
$im->moveTo(0,0);
$im->lineTo($d, 190);

$gif = $anim->gif_data;
ok $gif, 'gif data ok';

my ($w, $h) = $anim->get_bounds;
is $w, 200, 'anim width ok';
is $h, 250, 'anim height ok';

($w, $h) = $im->getBounds;
is $w, 200, 'im width ok';
is $h, 250, 'im height ok';

#exit(0);

for (1..6) {
    my $im = $anim->add_frame;
    %colors = $anim->color_map;
    #$im->bgcolor($colors{white});
    #$im->ellipse(5, 5, $d, $d, $colors{green});

    $im->bgcolor($colors{white});
    $im->fgcolor($colors{red});

    $d += 5;
    $im->moveTo(0, 0);
    #warn "$d $d";
    $im->lineTo($d, 190);

    my $new_gif = $anim->gif_data;
    ok $new_gif, 'gif data ok';
    ok length($new_gif) > length($gif), 'gif data grows';
}

my $out;
ok open($out, "> t/simple.gif"), 'file creation ok';
binmode $out;
print $out $anim->gif_data;
ok close $out;
ok -f 't/simple.gif';
#warn "ok";

is File::Compare::compare('t/simple.gif', 't/~simple.gif'), 0;
