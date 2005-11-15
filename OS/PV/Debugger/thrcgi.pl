use warnings;
use strict;

use HTTP::Server::Simple;
use Getopt::Std;

my %opts;
getopts('a:r:', \%opts);

my $astfile = $opts{a};
die "error: No AST file specified via -a option.\n"
    if !$astfile;

my $resfile = $opts{r};
die "error: No result file specified via -r option.\n"
    if !$resfile;

my $ast = Template::Ast->read($astfile) or
    die Template::Ast->error;

my @steps = parse_res($resfile);

my $server = MyServer->new();
$server->run();

sub parse_res {
    my $fname = shift;
    open my $in, $resfile or
        die "error: Can't open $resfile for reading: $!\n";
    my @steps;
    my %focuses;
    while (<$in>) {
        if (/^#-# (\d*)..(.+)/) {
            my ($count, $active) = ($1, $2);
            $focuses{$active} = $count;
            my $r = { focuses => {%focuses}, active => $active };
            push @steps, $r;
        }
    }
    close $in;
    return @steps;
}

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
        dump_listing($cgi);
    } elsif ($url =~ /logo.png$/) {
        dump_file($cgi, 'logo.png');
    } elsif ($cgi->param('next')) {
        if ($url =~ /step(\d+)/) {
            my $step = $1;
            dump_listing($cgi, $step);
        } else {
            warn "Not found.\n";
        }
    } elsif ($cgi->param('restart')) {
        dump_listing($cgi);
    }
}

sub dump_listing {
    my ($cgi, $step) = @_;
    $step = 0 unless defined $step;
    print "HTTP/1.0 200 OK\n";
	print $cgi->header(-type=>'text/html', -charset=>'gb2312');
    if ($step >= @steps) {
        $step = 0;
    }
    my $tt = Template->new;
    $tt->process(
        'listing.tt',
        { focuses => $steps[$step]->{focuses},
          active => $steps[$step]->{active},
          ast => $ast,
          step_id => ++$step },
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
