#: interp-cgi.pl
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-16 2005-11-16

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
use Interp::Newton::DiffQuots;

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
    elsif ($url =~ /eval/) {
        my ($x, $y) = ($cgi->param('x'), $cgi->param('y'));
        $x =~ s/^\s+|\s+$//g;
        $y =~ s/^\s+|\s+$//g;
        my @x = split(/\s+/, $x);
        my @y = split(/\s+/, $y);
        if (!@x or !@y or @x != @y) {
            dump_home($cgi, \@x, \@y, '有错误发生：坐标 x 的个数必须等于 f(x) 的个数');
        } else {
            dump_result($cgi, \@x, \@y);
        }
    }
}

sub dump_home {
    my ($cgi, $x, $y, $msg) = @_;
    print "HTTP/1.0 200 OK\n";
	print $cgi->header(-type=>'text/html', -charset=>'gb2312');
    my $tt = Template->new;
    $tt->process(
        'tpl/home.tt',
        { step => 0, Xs => $x, Ys => $y, msg => $msg },
        \*STDOUT,
    ) || warn $tt->error();
}

sub dump_result {
    my ($cgi, $x, $y) = @_;
    my $newton = Interp::Newton::DiffQuots->new(Xs => $x, Ys => $y);
    my @cols = $newton->diff_quot;
    my $poly = $newton->polynomial;
    my $poly2 = $newton->maple->eval("collect($poly,x)");
    my $res = $newton->test_polynomial($poly) ? 'pass' : 'fail';
    print "HTTP/1.0 200 OK\n";
	print $cgi->header(-type=>'text/html', -charset=>'gb2312');
    my $tt = Template->new;
    $tt->process(
        'tpl/home.tt',
        { step => 1, data => [ $x, @cols ], Xs => $x, Ys => $y,
          poly => $poly, simple_poly => $poly2, test_res => $res},
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
