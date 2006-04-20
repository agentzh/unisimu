#: ipinfo.pl
#: Print out the geographical location of a given IP
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-20 2006-04-20

use strict;
use warnings;

use LWP::UserAgent;
use Net::DNS;
use Template::Extract;

my $id = shift or
    die "Usage:\n\tipinfo <ip-address>\n\tipinfo <domain>\n";

$id =~ s,^https?://,,;
if ($id =~ m/^\d+ (?: \.\d+ ){3}$/x) {
    ip_info($id);
} else {
    my @ips = domain2ip($id);
    warn "    info: $id resolved to ", jj(@ips), ".\n";
    for my $ip (@ips) {
        ip_info($ip);
    }
}

sub ip_info {
    my ($ip) = @_;
    my $agent = LWP::UserAgent->new;
    #$agent->env_proxy();

    my $res = $agent->post('http://whois.ipcn.org/', {q => $ip});
    my $html;
    if ($res->is_success) {
        $html = $res->content;
    } else {
        die $res->status_line;
    }
    $html =~ s/\r//g;

    my $obj = Template::Extract->new;
    my $template = << '.';
<tr><td>您查询的IP是：</td><td>[% ip %]</td><td>对方所在地：</td><td>[% location %]</td></tr></table>
.

    my $data = $obj->extract($template, $html);
    if ($data) {
        print "IP Address:        ", $data->{ip}, "\n";
        print "Physical Location: ", $data->{location}, "\n";
    } else {
        die "Oops! Page template changed!";
    }
}

sub domain2ip {
    my ($domain) = @_;
    my $dns = Net::DNS::Resolver->new;
    my $query = $dns->search($domain);

    my @ips;
    if ($query) {
        foreach my $rr ($query->answer) {
            next unless $rr->type eq "A";
            push @ips, $rr->address;
        }
    } else {
        warn "DNS resolving failed: ", $dns->errorstring, "\n";
    }
    return @ips;
}

sub jj {
    if (@_ == 1) { return $_[0]; }
    my @s = @_;
    $s[-1] = "and $s[-1]";
    return join(', ', @s);
}
