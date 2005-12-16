#: viewtex.pl
#: Pod Dynamic LaTeX Viewer
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-12-16 2005-12-16

use strict;
use warnings;
use Getopt::Std;
use Config;
use FindBin;

BEGIN {
    $ENV{CATALYST_ENGINE} ||= 'HTTP';
}

my %opts;
getopts('hfp:', \%opts);
my $fork = $opts{f} || 0;
my $port = $opts{p} || 3040;

if ($opts{h}) {
    print <<"_EOC_";
Usage: $FindBin::Script [-f] [-d] [-s <css-file>] [-p <port-number>]

Options:
    -h         print this help
    -f         handle each request in a new process
               (defaults to false)
    -p <port>  specify port number (defaults to 3040) 

_EOC_
    exit(0);
}

########################################################

package ViewTex;

use strict;
use warnings;

use Perl6::Slurp;
use File::Spec;

use Catalyst qw/
    -Debug
    Static::Simple
    Textile
/;
our $VERSION = '0.01';

__PACKAGE__->config(
    name => 'ViewTex',
    home => '.',
);

__PACKAGE__->setup(qw/Static::Simple/);

# forward to modlist by default
sub default : Private {
    my ( $self, $c ) = @_;
    $c->stash->{error} = 'No PDF file specified.';
    $c->forward('/err');
}

# show error messages
sub err : Private {
    my ( $self, $c ) = @_;
    #my $html = $c->stash->{error};
    my $html = $c->textile->process($c->stash->{error});
    $c->res->output($html);
}

# show PDF file directly
sub showpdf : Regex('(.+\.pdf)$') {
    my ( $self, $c ) = @_;
    my $file = $c->stash->{texfile} || $c->req->snippets->[0];
    my $in;
    if (not open($in, $file)) {
        $c->stash->{error} = "Can't open $file for reading: $!";
        $c->forward('/err');
        return;
    }
    my $content = slurp '<:raw', "$file";
    $c->res->content_type("application/pdf");
    $c->res->output($content);
    close $in;
}

# show tex file through PDF
sub showtex : Regex('(.+\.tex)$') {
    my ( $self, $c ) = @_;
    my $infile = $c->req->snippets->[0];

    (my $outfile = $infile) =~ s/\.tex$/.pdf/;
    warn "Outfile = $outfile";
    unlink $outfile if -f $outfile;

    my $dir;
    my @dirs = File::Spec->splitdir($infile);
    if (@dirs > 1) {
        pop @dirs;
        $dir = File::Spec->catfile(@dirs);
    } else {
        $dir = '.';
    }
    warn $dir;
    warn $infile;

    my $tmpdir = File::Spec->tmpdir;
    open my $pipe, "pdflatex -output-directory=$dir -aux-directory=$tmpdir $infile |";
    if (not $pipe) {
        $c->stash->{error} = "Can't spawn pdflatex: $!";
        $c->forward('/err');
        return;
    }
    warn <$pipe>;
    close $pipe;
    $c->stash->{texfile} = $outfile;
    $c->forward('/showpdf');
}

############################################################

ViewTex->run( $port, 'localhost', {
     argv         => [],
    'fork'        => $fork,
} );
