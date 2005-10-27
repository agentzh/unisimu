use warnings;
use strict;
use HTTP::Server::Simple;

my $server = MyServer->new();
$server->run();

package MyServer;
use warnings;
use strict;
use URI::Escape;

use base qw(HTTP::Server::Simple::CGI);

sub handle_request {
    my ($self, $cgi) = @_;     #... do something, print output to default
    my $base = $cgi->url;
    $base = quotemeta($base);
    #warn "base = $base\n";
    my $code = $cgi->self_url;
    $code =~ s,^$base/,,;
    print $cgi->start_html('CGI Evaluater');
    $code = uri_unescape($code);
    my $res = eval $code;
    warn "$code => $res\n";
    print $cgi->h1($@ ? $@ : "$code = $res");
    print $cgi->end_html;
}
