#: getmsn.pm
#: 2006-07-08 2006-07-10

use strict;
use warnings;

use WWW::Mechanize::Cached;
use Template::Extract;
#use Data::Dumper;

my $obj = Template::Extract->new;

my $tt_journal = << 'END_TEMPLATE';
[% FOREACH journals %]
<div id='entry[% ... %]'><table cellspacing=0 border=0 width='100%' class='blogpost'><tr>
<td class='bvh8'></td></tr>[% raw_date %]<tr><td class='ellipse'><span class=
'bvTitle' id='[% ... %]'>
[% title %]</span></td></tr><tr><td class='bvh8'></td></tr><tr><td id='[% ... %]'>
[% body %]</td></tr>[% /<tr><td><table cellspacing=0 border=0>|/ %]</table></td></tr><tr>
<td class='bvh8'></td></tr><tr><td width=100%>[% ... %]
<table cellspacing=0><tr><td valign=top nowrap>[% time %]</td>[% ... %]
<a href='[% link %]?_c11_blogpart_blogpart=blogview&_c=blogpart#permalink' title=
'Click to show the permalink for this entry.'>Permalink</a>[% ... %]</div></td></tr><tr>
<td class="line"></td></tr><tr><td class="bvh8"></td></tr></table></div>
[% END %]
END_TEMPLATE

$tt_journal =~ s/\n+//gs;
my $regex_journal = $obj->compile($tt_journal);
#warn $regex_journal;

my $tt_date = << 'END_TEMPLATE';
<tr><td valign=top><span class='bold' id='LastMDate[% ... %]'>[% date %]
</span></td></tr><tr>
END_TEMPLATE
$tt_date =~ s/\n+//gs;
my $regex_date = $obj->compile($tt_date);
#warn $regex_date;

  ##

sub next_page {
    my $agent = $_[0];
    $agent->find_link(text_regex => qr/Click to view next \d+ entries/i);
}

sub process_page {
    my $agent = $_[0];
    my $content = $agent->content;
    if (!$content) {
        die "No content obtained.\n";
    }
    $content = abs_img($agent, $content);
    my $data = {};
    $obj->run($regex_journal, $content, $data);
    if (!$data) {
        die "No data found.\n";
    }
    my @journals = @{$data->{journals}};
    my $prev_date;
    for my $journal (@journals) {
        my $raw_date = $journal->{raw_date};
        my $date;
        if ($raw_date) {
            my $match = {};
            $obj->run($regex_date, $raw_date, $match);
            $date = ($prev_date = $match->{date});
            #warn "!!! $date";
        } else {
            $date = $prev_date;
        }
        $journal->{date} = $date;
        delete $journal->{raw_date};
    }
    #print Data::Dumper::Dumper(\@journals);
    warn scalar @journals, " journal(s) processed.\n";
    @journals;
}

sub abs_img {
    my ($agent, $html) = @_;
    my @imgs = $agent->find_all_images();
    for my $img (@imgs) {
        my $abs_url = ''.$img->url_abs;
        my $rel_url = ''.$img->url;
        next if $rel_url =~ /^http:/ or $abs_url eq $rel_url;
        warn "  info: absolutizing '$rel_url'...\n";
        $html =~ s/"$rel_url"/"$abs_url"/;
    }
    $html;
}

1;
