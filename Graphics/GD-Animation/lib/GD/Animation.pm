#: GD/Animation.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-08 2006-05-08

package GD::Animation;

use GD::Simple;
use 5.006001;
use strict;
use warnings;
use Perl6::Attributes;

our $VERSION = '0.01';

sub new {
    my $class = shift;
    my $w = shift;
    my $h = shift;
    my %opts = @_;
    my %colors = %{ $opts{colors} };
    $opts{loops} ||= 0;
    if (!defined $opts{delay}) {
        $opts{delay} = 100;
    }
    my $self = bless {
        colors => \%colors,
        color_map => {},
        images => [],
        width => $w,
        height => $h,
        loops => $opts{loops},
        delay => $opts{delay},
    };
    my $i = 0;
    for my $key (sort keys %colors) {
        $.color_map{$key} = $i++;
    }
    $self;
}

sub _alloc_colors {
    my ($self, $im) = @_;
    for my $key (sort keys %.colors) {
        my $color = $.colors{$key};
        $.color_map{$key} = $im->colorAllocate(@$color);
    }
}

sub color_map {
    my ($self) = @_;
    %.color_map;
}

sub add_frame {
    my ($self, $src_im) = @_;
    my $im = GD::Simple->new($.width, $.height);
    $self->_alloc_colors($im);
    if (defined $src_im) {
        if ($src_im->isa('GD::Simple')) {
            $src_im = $src_im->gd;
        }
        $im->copy(
            $src_im, 0, 0, 0, 0, $.width, $.height,
        );
    }
    push @.images, $im;
    $im;
}

sub get_bounds {
    my $self = shift;
    ($.width, $.height);
}

sub gif_data {
    my $self = shift;
    my $data = $.gif_data;
    my @images = @.images;
    if (@images) {
        my $gif = $images[0]->gifanimbegin(1, $.loops);
        for my $im (@images) {
            $gif .= $im->gifanimadd(1, 0, 0, $.delay, 1);
        }
        $gif .= $images[-1]->gifanimend;
        return $gif;
    } else {
        return undef;
    }
}

1;
__END__

=head1 NAME

GD::Animation - Class to generate Gif Animation Images

=head1 SYNOPSIS

    use GD::Animation;
    my $anim = GD::Animation->new(
        200, 250,
        loops => 0,
        colors => {
            white => [255, 255, 255],
            red   => [255, 0, 0],
        },
    );
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

    binmode \*STDOUT;
    print $anim->gif_data;

=head1 METHODS

=over

=item $anim = GD::Animation->new($width, $height, ...)

The constructor accepts at least two arguments. The first two
are for width and height. There are two optional named arguments,
C<loops>, C<colors>, and C<delay>.

If C<loops> is set to negative numbers, the animation won't loop
at all. And if C<loops> is set to 0, the animation loop forever.
Positive values of C<loops> cause the animation repeat for
the specified times. Here are some examples:

    $anim = GD::Animation->new(100, 200, loops => 0); # loop forever
    $anim = GD::Animation->new(100, 200, loops => -1); # no loops
    $anim = GD::Animation->new(100, 200, loops => 3); # repeat 3 times

The C<loops> option is default to 0.

The C<colors> named arguments are also very important. It specifies
the color maps used by every frames in the animation object. An example
is given below:

    $anim = GD::Animation->new(
        100, 200,
        colors => {
            white => [255, 255, 255],
            black => [0, 0, 0],
            red   => [255, 0, 0],
        },
    );

The assigned indices of the colors can be accessed later by calling
the C<color_map> method.

The C<delay> option specifies the delay (in ms) between adjacent frames,
default to 1 sec (100 ms). For example,

    $anim = GD::Animation->new(
        100, 200,
        delay => 10,
        colors => { ... },
    );

will make the animation much more smooth.

=item $image = $anim->add_frame()

Creates a new GD::Simple object and add it to the animation as the next
frame. The C<add_frame> method allocates color map automatically so you
don't have to worry about it.

=item $new_image = $anim->add_frame($image)

Copies the content of a GD::Simple or GD::Image instance C<$image> to a
new GD::Simple object, appends the latter to the animation, and returns
the new frame.

=item %colors = $anim->color_map()

This method returns a hash whose keys are color names and whose values
are the corresponding color index.

=item ($width, $height) = $anim->get_bounds

Returns the width and height of every frame in the animation.

=item $gif_data = $anim->gif_data

Returns the gif file binary data of the whole animation.

=back

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang. All rights reserved.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.
