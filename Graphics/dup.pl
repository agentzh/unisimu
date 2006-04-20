use strict;
use warnings;
use GD;
use GD::Simple;

my $file = shift or die "Usage: dup <image-file>";
my $in = GD::Image->new($file) or die "$!";
my ($w, $h) = $in->getBounds();
my $out = GD::Simple->new($w, $h);
my %colors;
for my $x (0..$w-1) {
    for my $y (0..$h-1) {
        my $index = $in->getPixel($x, $y);
        my ($r, $g, $b) = $in->rgb($index);
        my $color;
        if ($colors{"$r-$g-$b"}) {
            $color = $colors{"$r-$g-$b"};
        } else {
            $color = $out->colorAllocate($r, $g, $b);
            $colors{"$r-$g-$b"} = $color;
        }
        $out->setPixel($x, $y, $color);
    }
}
binmode STDOUT;
print ($out->png);
