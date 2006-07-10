#!wperl

use strict;
use warnings;

use HTTP::Proxy ':log';
use LWP::UserAgent;
use FindBin;

my $logfile = "myproxy.log";

my $exclude_pat = shift || '\.gif$ |\.jpeg$ |\.css$ |\.js$';

package My::HTTP::Proxy;

use strict;
use warnings;
use base 'HTTP::Proxy';

sub log {
    my $self  = shift;
    my $msg = $_[2];
    return if $msg and $msg =~ /$exclude_pat/x;
    my $retval = $self->SUPER::log(@_);
    close $self->logfh;
    open my $fh, ">> $logfile" or
        die "Can't reopen $logfile for appending: $!";
    $self->logfh($fh);
    $retval;
}

package main;

open my $log, "> $logfile" or
    die "Can't open $logfile for writing: $!";

$log->autoflush(1);

my $proxy = My::HTTP::Proxy->new(
    logmask => STATUS,
    logfh => $log,
);

$proxy->max_clients(100);

#my $agent = LWP::UserAgent->new(
#    env_proxy  => 1,
#    timeout => 200,
#);

#$proxy->agent($agent);
$proxy->host(undef);
$proxy->port(3128);

print "Starting the proxy...\n";
$proxy->start();
