#: qooqle.pl
#: Web interface for qqBase
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-06 2005-11-06

use strict;
use warnings;

use HTTP::Server::Simple;
use DBI;

my $time = 0;
my $server = MyServer->new();
$server->run();

package MyServer;
use warnings;
use strict;
use URI::Escape;
use Template;
use CGI::Carp qw(fatalsToBrowser);
use POSIX qw(strftime);
use Time::HiRes;

use base qw(HTTP::Server::Simple::CGI);

our $WholeWord = 0;

sub handle_request {
    $time = Time::HiRes::time;
    my ($self, $cgi) = @_;     #... do something, print output to default
    my $base = $cgi->url;
    $base = quotemeta($base);
    #warn "base = $base\n";
    my $url = $cgi->self_url;
    $url =~ s,^$base/,,;
    $url =~ s/\?.*$//g;
    $url = uri_unescape($url);
    $url = '' if !$url;
    warn "URL: $url\n";

    #my @params = $cgi->param;
    #warn "@params";
    $WholeWord = $cgi->param('wholeword');
    if ($url eq '/' or $url eq '') {
        if (my $query = $cgi->param('query')) {
            warn "Query: $query\n";
            my @words;
            while ($query =~ s/"\s*([^"]*)"//) {
                push @words, $1;
            }
            push @words, split /\s+/, $query;
            @words = grep !/^\s*$/, @words;
            map s/\s*$//, @words;
            local $" = ':';
            warn "Words: @words\n";
            if (!@words) {
                dump_home($cgi);
            } else {
                dump_list($cgi, @words);
            }
        }
        else {
            dump_home($cgi);
        }
    }
    elsif ($url =~ m[^/?join]) {
        warn "Joining...\n";
        my @sessions = $cgi->param('session');
        dump_sessions($cgi, @sessions);
    }
    elsif ($url =~ m[^/?display]) {
        warn "Displaying...\n";
        dump_sessions($cgi, $cgi->param('session'));
    }
    else {
        dump_file($cgi, $url);
    }
}

sub dump_home {
    my $cgi = shift;
    dump_file($cgi, 'index.html');
}

sub dump_list {
    my @cgi = shift;
    my @words = @_;
    my $query = <<_EOC_;
select session_id, U1.user_name as sender, U2.user_name as receiver, msg_body
from msgs, users as U1, users as U2
where msgs.msg_from=U1.user_id and msgs.msg_to=U2.user_id and
_EOC_
    $query .= '(' . (join ' or ', (map { "msg_body like '%$_%'" } @words)) . ')';
    $query .= "\norder by session_id asc";
    warn "SQL: $query\n";

	my $dbh = connect_db();
	my $sth = $dbh->prepare($query);
    $sth->execute;

    my @hits;
    while (my $ref = $sth->fetchrow_hashref) {
        #warn "REF: $ref\n";
		my $msg = $ref->{msg_body};
		my $matched = 0;
		for my $word (@words) {
			my $pat = quotemeta($word);
			if ($word =~ m/\W/ or !$WholeWord) {
				$msg =~ s,$pat,<B>$&</B>,isg;
				$matched = 1;
			}
			elsif ($WholeWord and $msg =~ s,\b$pat\b,<B>$&</B>,isg) {
				$matched = 1;
			}
		}
		next if !$matched;

		$msg = quote_html($msg);
        my $rec = {
            'time'   => scalar localtime($ref->{session_id}),
            session  => $ref->{session_id},
            receiver => $ref->{receiver},
            sender   => $ref->{sender},
            message  => $msg,
        };
        push @hits, $rec;
    }
    #warn "\@hits: ", scalar(@hits), "\n";
    my $elapsed = sprintf("%.2f", Time::HiRes::time - $time);
    my $tt = Template->new;
    print "HTTP/1.0 200 OK\n\n";
    $tt->process(
        'tpl/list.tt',
        { whole_word => $WholeWord, query => join(' ', @words), 'elapsed' => $elapsed, hits => \@hits },
        \*STDOUT,
    ) || warn $tt->error();
    $dbh->disconnect;
}

sub dump_sessions {
    my $cgi = shift;
    my @sessions = @_;
    my $query = <<_EOC_;
select session_id, msg_time, U1.user_name as sender, U2.user_name as receiver, msg_body
from msgs, users as U1, users as U2
where msgs.msg_from=U1.user_id and msgs.msg_to=U2.user_id and
_EOC_
    $query .= '(' . (join ' or ', (map { "session_id=$_" } @sessions)) . ')';
    $query .= "\norder by session_id, offset asc";
    warn "SQL: $query\n";

	my $dbh = connect_db();
	my $sth = $dbh->prepare($query);
    $sth->execute;
    
    my $session;
    my @hits;
    print "HTTP/1.0 200 OK\n";
    while (my $ref = $sth->fetchrow_hashref) {
        my $rec = {
            session  => $ref->{session_id},
            session_time => strftime("%Y-%m-%d %a", localtime($ref->{msg_time})),
            msg_time => strftime("%H:%M:%S", localtime($ref->{msg_time})),
            receiver => $ref->{receiver},
            sender   => $ref->{sender},
            message  => $ref->{msg_body},
        };
        push @hits, $rec;
    }
    #warn "\@hits: ", scalar(@hits), "\n";
    my $tt = Template->new;
    $tt->process('tpl/session.tt', { hits => \@hits }, \*STDOUT)
      || warn $tt->error();
    $dbh->disconnect;
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

sub quote_html {
	my $src = shift;
	$src =~ s,>,&gt;,g;
	$src =~ s,<,&lt;,g;
	$src =~ s,&lt;B&gt;,<B>,g;
	$src =~ s,&lt;/B&gt;,</B>,g;
	$src =~ s/\n/<br>/gs;
	$src =~ s/\t/    /g;
	$src =~ s/ /&nbsp;/g;
	return $src;
}

sub connect_db {
	my $dsn = $ENV{DSN};
	die "No env DSN set. So no database is available\n" unless $dsn;
    my $dbh = DBI->connect($dsn, { PrintError => 1, RaiseError => 0 });
	return $dbh;
}
