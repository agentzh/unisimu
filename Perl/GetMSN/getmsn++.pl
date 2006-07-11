use strict;
use warnings;

use WWW::Mechanize::Cached;
use WebCache;
use Template::Extract;
use YAML::Syck;
use Getopt::Std;
use Data::Dumper;

my %opts;
getopts('t:da', \%opts);

$WebCache::RefreshCache = !$opts{d};

my $name = shift or die "Usage: getmsn++ [-d] [-t <number>] [-a] <username>";
$name =~ s/\.yml$//;
my $journals = LoadFile("$name.yml");
if (!$journals) {
    die "error: no data loaded from $name.yml.\n";
}

my $top = $opts{t} || 5;
if ($opts{a}) {
    $top = 1000 * 1000; # maybe we need a better way to do this. :P
}

my $cache = WebCache->new;
my $agent = WWW::Mechanize::Cached->new( cache => $cache, autocheck => 1 );
$agent->env_proxy();

my $tt_comment = << 'END_TEMPLATE';
[% FOREACH comments %]
[% ... %]
<td align='left' class='bvwordwrap'>[% body %]</td>
</tr><tr><td height=8 colspan='2'></td></tr>
<tr class='commententry'><td colspan='2'>
<span class='notgray'>Published By</span> <span class='bold mstitlewrap'>
[% author %]</span><span class='notgray'>&nbsp;-&nbsp;[% time %]</span></td></tr>
[% ... %]
[% END %]
END_TEMPLATE

$tt_comment =~ s/\n+//gs;
my $obj = Template::Extract->new;
my $regex_comment = $obj->compile($tt_comment);

my $tt_author = '[% author %] <a href="[% link %]" target="_blank" rel="nofollow">';
my $regex_author = $obj->compile($tt_author);

my $i = 0;
for my $journal (@$journals) {
    if (++$i > $top) {
        undef $WebCache::RefreshCache;
    }
    my $link = $journal->{link};
    my $res = $agent->get($link);
    my $page = $agent->content;
    my $data = {};
    $obj->run($regex_comment, $page, $data);
    my $comments = $data->{comments};
    if ($comments) {
        #print $page;
        for my $comment (@$comments) {
            my $author = $comment->{author};
            my $match = {};
            $obj->run($regex_author, $author, $match);
            if ($match->{author}) {
                $comment->{author} = $match->{author};
                $comment->{author_url} = $match->{link};
            }
        }
        #print Dumper($comments);
        $journal->{comments} = $comments;
    } else {
        if ($page =~ /Published By/) {
            warn "Template failed to match ($link).\n";
        } else {
            #warn "  info: no comments for this entry.\n";
        }
    }
}

DumpFile("$name++.yml", $journals);
warn "$name++.yml generated.\n";
