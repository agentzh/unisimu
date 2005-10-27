use warnings;
use strict;
use HTTP::Server::Simple;

$MyServer::count = 0;
my $server = MyServer->new();
$server->run();

package MyServer;
use warnings;
use strict;
use Config;
use URI::Escape;
use GD::Stack;

use base qw(HTTP::Server::Simple::CGI);
my $stack;

my $old_uri = '';
our $count;

sub assign {
    restart() if !defined $stack;
    $stack->draw_used_block(@_);
}

sub restart {
    $stack = GD::Stack->new(100,200,450);
}

sub handle_request {
    my ($self, $cgi) = @_;     #... do something, print output to default
    my $base = $cgi->url;
    $base = quotemeta($base);
    #warn "base = $base\n";
    my $code = $cgi->self_url;
    $code =~ s,^$base/,,;
    $code = uri_unescape($code);
    $code = 'restart' if !$code;
    my $res = eval $code;
    warn "IMG FILE: $imgfile\n";
    my $data;
    if (!$@) {
        $data = $stack->as_png();
    }
    print "HTTP/1.0 200 OK\r\n";
    print $cgi->header(
        -type=>'image/png',
        -expires=>'0');
    binmode \*STDOUT;
    print $data;
}
