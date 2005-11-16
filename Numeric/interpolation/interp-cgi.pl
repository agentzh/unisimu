#: interp-cgi.pl

use strict;
use warnings;

use HTTP::Server::Simple;
my $server = MyServer->new();
$server->run();

package MyServer;

use warnings;
use strict;
use URI::Escape;
use Template;
use Template::Ast;

use base qw(HTTP::Server::Simple::CGI);

sub handle_request {
    my ($self, $cgi) = @_;
    my $base = $cgi->url;
    $base = quotemeta($base);
    #warn "base = $base\n";
    my $url = $cgi->self_url;
    $url =~ s,^$base/,,;
    $url = uri_unescape($url);
    $url = '' if !$url;
    warn "URL: $url\n";
    if ($url eq '') {
        dump_home($cgi);
    }
    elsif ($url =~ /\.png$/) {
        dump_file($cgi, $url);
    }
}

sub dump_home {
    my $cgi = shift;
    print "HTTP/1.0 200 OK\n";
	print $cgi->header(-type=>'text/html', -charset=>'gb2312');
    my $tt = Template->new;
    $tt->process(
        'tpl/home.tt',
        { a => 'a' },
        \*STDOUT,
    ) || warn $tt->error();
}

sub dump_file {
    my ($cgi, $fname) = @_;
    my $in;
    if (!open $in, $fname) {
        warn "Internal Server Error: Resource $fname not found: $!";
        return;
    }
    local $/;
    binmode $in;
    my $content = <$in>;
    close $in;
    print "HTTP/1.0 200 OK\n\n";
    print $content;
}
