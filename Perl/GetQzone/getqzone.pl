#: getqzone.pl
#: Access Qzone sites to collect data
#: Copyright (c) 2006 Agent Zhang
#: 2006-07-10 2006-07-10

use strict;
use warnings;

use getqzone;

use WWW::Mechanize::Cached;
use WebCache;
use YAML::Syck;
use Getopt::Std;

my $main_url =
    "http://u13.qzone.qq.com/cgi-bin/cgi_qqzone_static.cgi?uin=##&flag=-1";

my $main_url2 =
    "http://u13.qzone.qq.com/cgi-bin/cgi_qqzone.cgi?uin=##&".
    "flag=-1758952704&maindomain=user113.qzone.qq.com&".
    "blog=b1.qzone.qq.com&msgboard=m1.qzone.qq.com&".
    "music=qzone-music.qq.com&photo=d2.photo.qq.com";

#$main_url = $main_url2;

my $title_url =
    "http://b1.qzone.qq.com/cgi-bin/blog/blog_one_title.cgi?uin=##&blogid=##&flag=0";

my $body_url =
    "http://b1.qzone.qq.com/cgi-bin/blog/blog_commentlist.cgi?uin=##&blogid=##&archive=-2";

my $home_url =
    "http://u13.qzone.qq.com/cgi-bin/cgi_client_entry.cgi?uin=##";

my %opts;
getopts('d', \%opts);

$WebCache::RefreshCache = !$opts{d};

my $qq_number = shift;
if (!$qq_number || $qq_number !~ /^\d+$/) {
    die "Usage: getqzone [-d] <qq-number>";
}

$main_url  =~ s/uin=##/uin=$qq_number/;
$main_url2 =~ s/uin=##/uin=$qq_number/;
$title_url =~ s/uin=##/uin=$qq_number/;
$body_url  =~ s/uin=##/uin=$qq_number/;
$home_url  =~ s/uin=##/uin=$qq_number/;

my $cache = WebCache->new;
my $agent = WWW::Mechanize::Cached->new( cache => $cache, autocheck => 1 );
$agent->env_proxy();

my $out_ast = {};
process_main($agent, $main_url, $main_url2, $out_ast);
warn "  info: ", scalar @{ $out_ast->{msg_board} }, " message(s) found in message board.\n";

process_articles($agent, $title_url, $body_url, $out_ast);
warn "  info: ", scalar @{ $out_ast->{blogs} }, " article(s) found in blogs.\n";

$out_ast->{link} = $home_url;
my $outfile = "$qq_number.yml";
DumpFile($outfile, $out_ast);
warn "$outfile generated.\n";
