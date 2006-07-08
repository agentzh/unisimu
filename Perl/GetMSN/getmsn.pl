use strict;
use warnings;

use getmsn;
use WebCache;
use YAML::Syck;
use Getopt::Std;

my %opts;
getopts('d', \%opts);

$WebCache::RefreshCache = !$opts{d};

my $name = shift or die "Usage: getmsn <username>";

my $cache = WebCache->new;
my $agent = WWW::Mechanize::Cached->new( cache => $cache, autocheck => 1 );
$agent->env_proxy();
$agent->get("http://$name.spaces.msn.com/blog");
my @journals = process_page($agent);

while (my $link = next_page($agent)) {
    #warn $link;
    $agent->get($link);
    push @journals, process_page($agent);
}

warn "for total ", scalar @journals, " journals processed.\n";
my $outfile = "$name.yml";
DumpFile($outfile, \@journals);
warn "  info: $outfile generated.\n";

#print $agent->content,"\n";
#my @links = $agent->find_all_links(text_regex => qr/Read comments \(\d+\)/);
#print "@links";
