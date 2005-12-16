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

use Catalyst qw/
    -Debug
    Static::Simple
    Session
    Session::Store::File
    Session::State::Cookie
    Textile
/;
our $VERSION = '0.01';

__PACKAGE__->config(
    name => 'ViewTex',
    home => '.',
);

__PACKAGE__->setup(qw/Static::Simple Textile/);

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

# show POD file with explicit path
sub show : Regex('(.+\.pdf)$') {
    my ( $self, $c ) = @_;
    my $file = $c->req->snippets->[0];
    #my $dir = File::Spec->updir($file);
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

############################################################

ViewTex->run( $port, 'localhost', {
     argv         => [],
    'fork'        => $fork,
} );
