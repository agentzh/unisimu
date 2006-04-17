#: imcmp.pl
#: Compare two (very simple) images
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-17 2006-04-17

use strict;
use warnings;
use List::Util qw(min);
use GD;
use Text::Table;

my ($file1, $file2) = @ARGV;
if (!$file1 || !$file2) {
    die "Usage: imcmp <image1> <image2>\n";
}

my $tb = Text::Table->new(
    "Coordinates", $file1, $file2,
);

my $im1 = GD::Image->new($file1) or die "$!";
my $im2 = GD::Image->new($file2) or die "$!";

my ($w1, $h1) = $im1->getBounds();
my ($w2, $h2) = $im2->getBounds();

my $w = min($w1, $w2);
my $h = min($h1, $h2);

my $diff = 0;
for my $x (0..$w-1) {
    for my $y (0..$h-1) {
        my $c1 = get_color($im1, $x, $y);
        my $c2 = get_color($im2, $x, $y);
        if ($c1 ne $c2) {
            $tb->add("($x, $y)", $c1, $c2);
            $diff = 1;
        }
    }
}

if ($diff) {
    print $tb->rule( '-', '+' );
    print $tb->title;
    print $tb->rule( '-', '+' );
    print $tb->body;
    print $tb->rule( '-', '+' );
}

sub get_color {
    my ($im, $x, $y) = @_;
    my $index = $im->getPixel($x, $y);
    my ($r, $g, $b) = $im->rgb($index);
    my $c = rgb2name($r, $g, $b);
    return $c;
}

sub rgb2name {
    my ($r, $g, $b) = @_;
    my $c = "<$r, $g, $b>";
    if ($c eq '<255, 255, 255>') {
        return 'white';
    } elsif ($c eq '<255, 0, 0>') {
        return 'red';
    } elsif ($c eq '<0, 0, 0>') {
        return 'black';
    } elsif ($c eq '<0, 255, 0>') {
        return 'green';
    } elsif ($c eq '<0, 0, 255>') {
        return 'blue';
    } else {
        return "<$r, $g, $b>";
    }
}
