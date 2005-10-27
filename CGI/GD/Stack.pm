package GD::Stack;

use strict;
use warnings;
use GD::Simple;

# GD::Stack->new($stack_sz, $img_width, $img_height)
sub new {
    my $class = shift;
    my $max = shift;
    my ($width, $height) = @_;
    my $img = GD::Simple->new(@_);
    my $self = bless {
        _width => $width,
        _height => $height,
        _img => $img,
        _scale => ($height-20)/$max,
    }, $class;
    $self->cyclic(0,$max);
    return $self;
}

sub draw_label {
    my ($self, $label, $top, $height) = @_;
    my $scale = $self->{_scale};
    my $width = $self->{_width};
    $top *= $scale; $height *= $scale;
    my $img = $self->{_img};
    $img->moveTo($width/2, 10+$top+$height/2+7);
    #$img->font('Times:italic');
    $img->fontsize(20);
    #$img->angle(-90);
    $img->string($label);    # some turtle graphics
}

sub draw_used_block {
    my $self = shift;
    my $label = shift;
    $self->draw_block('cyan', @_);
    $self->draw_label($label, @_);
}

sub cyclic {
    shift->draw_block('white', @_);
}

sub draw_block {
    my $self = shift;
    my ($color, $top, $height) = @_;
    my $img = $self->{_img};
    my $scale = $self->{_scale};
    $top *= $scale; $height *= $scale;
    $img->bgcolor($color);
    $img->fgcolor('blue');
    $img->rectangle(50,10+$top,150,10+$top+$height);
}

sub as_png {
    my $self = shift;
    my $file = shift;
    my $data = $self->{_img}->png;
    if ($file) {
        open my $out, ">$file" or
            die "Can't open $file for writing: $!\n";
        binmode $out;
        print $out $data;
        close $out;
        return 1;
    } else {
        return $data;
    }
}

1;
