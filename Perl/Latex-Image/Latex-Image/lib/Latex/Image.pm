#: Latex/Image.pm
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-19 2005-11-19

package Latex::Image;

use strict;
use warnings;
use File::Spec;

our $VERSION = '0.01';
our $error = '';

sub error {
    return $error;
}

sub convert {
    shift;
    my $latex = shift;
    my $imfile = shift;
    my %opts = @_;
    my $density = $opts{density};
    $density ||= '200x200';
    my $border = $opts{border};
    $border ||= 5;
    (my $texfile = $imfile) =~ s/\.[^\.]+$/.tex/;
    (my $pdffile = $imfile) =~ s/\.([^\.])+$/.pdf/;

    warn "GIF image won't be cropped. Choose PNG or JPEG instead.\n"
        if $1 =~ /^gif$/i;

    open my $out, ">$texfile" or
        die "error: Can't open $texfile for reading: $!\n";
    my $src = <<_EOC_;
\\documentclass{article}
\\pagestyle{empty}
\\pdfoutput=1
\\usepackage{amsmath}
\\begin{document}
$latex
\\end{document}
_EOC_
    print $out $src;
    close $out;

    my $dir;
    my @dirs = File::Spec->splitdir($imfile);
    if (@dirs > 1) {
        pop @dirs;
        $dir = File::Spec->catfile(@dirs);
    } else {
        $dir = '.';
    }

    my $tmpdir = File::Spec->tmpdir;
    if (system("pdflatex -quiet -output-directory=$dir -aux-directory=$tmpdir $texfile") != 0) {
        $error = "Conversion from LaTex to PDF failed.\n".
            "    (Make sure pdflatex is ready.)\n";
        return undef;
    }
    if (system("convert -trim -density $density -border $border -bordercolor white $pdffile $imfile") != 0) {
        $error = "Conversion from PDF to PNG failed.\n".
            "    (Make sure convert is ready.)\n";
        return undef;
    }
    if (!-e "$imfile") {
        $error = "error: $imfile not found.\n";
        return undef;
    }

    unlink $texfile;
    unlink $pdffile;
    return 1;
}

1;
__END__

=head1 NAME

Latex::Image - Convert Latex string to image files

=head1 SYNOPSIS

  use Latex::Image;

  Latex::Image->convert(
    'So we get $\sqrt {a^2  + b^2 }$.',
    'foo.png'
  ) or die Latex::Image::error;

  $latex = <<'_EOC_';
  \[
    \frac{{n!}}
    {{r!\left( {n - r} \right)!}}
  \]
  _EOC_
  Latex::Image->convert($latex, 'equ.jpg', density=>'100x100') or
    die Latex::Image->error;

  # Don't choose .gif file, since it won't be cropped.

=head1 DESCRIPTION


=head2 EXPORT

None by default.

=head1 SEE ALSO

L<Image::Magick>

=head1 AUTHOR

Agent Zhang, E<lt>agent2002@126.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 Agent Zhang

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
