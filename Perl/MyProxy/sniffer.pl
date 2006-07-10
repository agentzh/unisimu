#: sniffer.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-07 2006-04-20

use strict;
use warnings;

use HTTP::Proxy;
use HTTP::Response;
use Getopt::Std;

my %opts;
getopts('hp:oive', \%opts);

if ($opts{h}) {
    print <<".";
Usage: sniffer [-hioe] [-p <port-number>] [log-file]

Options:
    -h                 print this help to stdout
    -p <port-number>   specify the port number of
                       the proxy
    -i                 log the request string
    -o                 log the response string
    -e                 use *_proxy env
    -v                 being verbose
.

    exit(0);
}

my $logfile = shift;
$logfile ||= "sniffer.log";
unlink $logfile;

my $port = $opts{p} || 3128;

my $agent = MyUA->new(
    env_proxy  => $opts{e},
    timeout => 100,
    file => $logfile,
);

my $proxy = HTTP::Proxy->new;

warn "Log file: $logfile\n";
$proxy->max_clients(100);
$proxy->agent( $agent );

# you may need to set the host
$proxy->host(undef);
$proxy->port($port);

print "Starting the proxy 127.0.0.1:3128...\n";
$proxy->start();

package MyUA;

use strict;
use warnings;
use File::Slurp;

use base 'LWP::UserAgent';

sub new {
    my $proto = shift;
    my %opts = @_;
    my $logfile = $opts{file};
    delete $opts{file};
    my $self = $proto->SUPER::new(%opts);
    $self->{file} = $logfile;
    return $self;
}

sub send_request {
    my $self = shift;
    my $request = shift;

    local $|;
    if ($self->{file} and $opts{i}) {
        my $str = $request->as_string() . "\n\n---\n\n";
        warn $str;
        write_file( $self->{file}, {append => 1}, $str );
    }
    my $response;
    eval {
        $response = $self->SUPER::send_request( $request );
    };
    if ($@ and not $response) {
        $response = HTTP::Response->new(500);
        warn $@;
    }
    if ($self->{file} and $opts{o}) {
        my $str = $response->as_string() . "\n\n---\n\n";
        warn $str if $opts{v};
        write_file( $self->{file}, {append => 1}, $str );
    }
    die "undef res" if not defined $response;
    return $response;
}
