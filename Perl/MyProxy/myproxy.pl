#!wperl

use strict;
use warnings;

use HTTP::Proxy ':log';
use HTTP::Response;
use FindBin;

my $env = 'MY_PROXY_HOME';
my $home = $ENV{MY_PROXY_HOME};
die "env $env not set" if not $home;

$| = 1;
my $logfile = ">>$home/myproxy.log";
open my $log, $logfile or
    die "Can't open $logfile for reading: $!";

my $proxy = HTTP::Proxy->new(
    logmask => STATUS,
    logfh => $log,
);

$proxy->max_clients(100);

my $agent = MyUA->new(
    env_proxy  => 1,
    timeout => 100,
);

$proxy->agent( $agent );

# you may need to set the host
$proxy->host(undef);
$proxy->port(3128);

while (1) {
    print "Starting the proxy...\n";
    eval {
        $proxy->start();
    };
    warn $@ if $@;
}

package MyUA;

use strict;
use warnings;
use HTTP::Proxy ':log';

use base 'LWP::UserAgent';

sub send_request {
    my $self = shift;
    my $request = shift;

    my $response;
    eval {
        $response = $self->SUPER::send_request( $request );
    };
    if ($@ and not $response) {
        $response = HTTP::Response->new(500, $@);
        warn $@;
    }
    if ($response->is_success) {
        my $type = $response->header('content-type');
        #warn $type;
        if ($type and $type =~ m[text/html]i) {
            if ($response->content =~ m[<title>\s*(.*\S)\s*</title>]si) {
                my $title = $1;
                $title =~ s/\n/ /gs;
                $proxy->log( STATUS, 'TITLE', $title);
            }
        }
    }
    return $response;
}

END {
    close $log;
}
