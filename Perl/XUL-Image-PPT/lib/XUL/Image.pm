package XUL::Image;

use 5.006001;
use Moose;

use Time::HiRes 'sleep';
use Win32::GuiTest qw( 
    FindWindowLike GetWindowText
    SetForegroundWindow SendKeys
);
use Clipboard;
use File::Spec;
use Image::Magick;

our $VERSION = '0.01';

has 'title'  => (is => 'ro', isa => 'Str', default => 'Mozilla');
has 'count'  => (is => 'ro', isa => 'Int', required => 1);
has 'delay'  => (is => 'rw', isa => 'Int', default => 1);
has 'outdir' => (is => 'rw', isa => 'Str', default => 'xul_img');

sub go {
    my $self = shift;
    my %args = @_;
    my @windows =
        grep { index(GetWindowText($_), $self->title) >= 0 } FindWindowLike(0, 'Mozilla');
    if (!@windows) {
        die "error: Can't find window titled '", $self->title, "'.";
    }
    if (@windows > 1){
        warn "warning: multiple mozilla windows found, will use the first one.\n";
    }
    SetForegroundWindow($windows[0]);
    sleep($self->delay);

    SendKeys("{F11}");
    sleep($self->delay);

    if ($args{reset}) {
        SendKeys("{HOME}");
        sleep($self->delay);
    }

    mkdir $self->outdir if !-d $self->outdir;
    my @files;
    for (1..$self->count) {
        SendKeys("{PRTSCR}{DOWN}");
        sleep($self->delay);
        my $fbase = sprintf("%03d", $_) . ".png";
        push @files, $fbase;
        my $outfile = File::Spec->catfile($self->outdir, $fbase);
        warn "generating $outfile...\n";
        my $imdata = Clipboard->paste;
        $self->crop_img($imdata, $outfile);
    }
    my $listing = File::Spec->catfile($self->outdir, 'listing.txt');
    open my $fh, "> $listing"
        or die "Cannot open $listing for writing: $!\n";
    print $fh join("\n", @files);
    close $fh;
    warn "$listing generated.\n";
    SendKeys("{F11}");
}

sub crop_img {
    my ($self, $imdata, $outfile) = @_;
    my $image = Image::Magick->new;
    $image->BlobToImage($imdata);
    my $ret = $image->Crop( geometry => '+0+33' );
    warn $ret if $ret;
    $ret = $image->Trim;
    warn $ret if $ret;
    $ret = $image->Write($outfile);
    warn $ret if $ret;
}

1;
__END__

=head1 NAME

XUL::Image - Converting XUL slides to Images

=head1 AUTHOR

Agent Zhang E<lt>agentzh@gmail.comE<gt>
Sal Zhong E<lt>zhongxiang721@gmail.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang. All rights reserved.
Copyright (c) 2006 Sal Zhong. All rights reserved.

This library is free software; you can redistribute it
and/or modify it under the same terms as perl itself.
