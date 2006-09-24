#: getqzone.pm
#: 2006-07-10 2006-07-18

use strict;
use warnings;

use List::Util 'max';
use XML::Simple;
use File::Slurp;
use Data::Dumper;

our ($TitleUrl, $BodyUrl, $LastBlog);

sub gb2utf {
    my $s = shift;
    from_to($s, "gb2312", "utf8");
    $s;
}

sub process_main {
    my ($agent, $main_url, $out_ast) = @_;
    my $xml = get_url($agent, $main_url);
    my $in_ast = XMLin($xml, ForceArray => ['item']);
    my $msg_board = $in_ast->{_x_22}->{rss}->{channel}->{item} || [];
    my @msgs;
    for my $msg (@$msg_board) {
        $msg->{author} =~ s/ >\Z|^\s+//g;
        $msg->{title}  =~ s/ >\Z|^\s+//g;
        my $new_msg = {
            body => $msg->{title},
            name   => $msg->{author},
            id     => $msg->{uin},
            date   => $msg->{pubDate},
        };
        push @msgs, $new_msg;
    }

    my $data = $in_ast->{_x_3}->{data};
    $data->{spacename} =~ s/\s*>\Z|^\s+//g;
    #print Dumper($data);
    my $personal_info = {
        age       => $data->{age},
        city      => $data->{city},
        desc      => $data->{desc},
        province  => $data->{province},
        birthday  => $data->{birthday},
        nickname  => $data->{nickname},
        interest  => $data->{interest},
        sex       => decode_sex($data->{sex}),
        bloodtype => $data->{bloodtype},
        spacename => $data->{spacename},
    };
    while (my ($key, $value) = each %$personal_info) {
        $personal_info->{$key} =~ s/ >\Z|^\s+//g if $value;
    }

    # debug purpose only:
    #for (qw<0 3 5 22>) {
    #    delete $in_ast->{"_x_$_"};
    #}
    #print Dumper($in_ast);

    my $recent_blogs = $in_ast->{_x_23}->{rss}->{channel}->{item};
    if (!$recent_blogs) {
        #warn $main_url2;
        $LastBlog = -1;
    } else {
        #warn Dumper($recent_blogs);
        $LastBlog = max keys %$recent_blogs;
        warn "  info: last blog's id is $LastBlog.\n";
    }

    $out_ast->{msg_board}      = \@msgs;
    $out_ast->{personal_info} = $personal_info;
    $out_ast;
}

sub decode_sex {
    my $code = shift;
    return 'M' if $code == 1;
    return 'F' if $code == 2;
    '';
}

sub process_articles {
    my ($agent, $title_url, $body_url, $out_ast) = @_;
    $TitleUrl = $title_url;
    $BodyUrl  = $body_url;
    my %blogs;
    my $count = 0;
    my $i = $LastBlog;
    while ($i >= 0) {
        if (++$count >= $::Top) {
            undef $WebCache::RefreshCache;
        }
        my $data = get_article($agent, $i--);
        next if !$data;
        my $category = $data->{category};
        $blogs{$category} ||= [];
        push @{ $blogs{$category} }, $data;
    }
    $out_ast->{blogs} = \%blogs;
    $out_ast->{nblogs} = $count;
    #print Dumper($out_ast);
    $out_ast;
}

sub get_article {
    my ($agent, $i) = @_;

    (my $url = $TitleUrl) =~ s/blogid=##/blogid=$i/;
    my $content = get_url($agent, $url);
    return undef if !$content;
    my $retval = { title_link => $url };
    unlink 'title.xml';
    if ($content =~ /<error\b/) {
        $retval->{title} = "Blog $i",
        $retval->{category} = gb2utf("个人日记");
    } else {
        #write_file('title.xml', $content, "\n<!-- $url -->\n");
        process_title($content, $retval);
    }

    ($url = $BodyUrl) =~ s/blogid=##/blogid=$i/;
    #warn $url;
    $content = get_url($agent, $url);
    #warn $content if $i == 4;
    unlink 'body.xml';
    return undef if !$content;
    #write_file('body.xml', $content, "\n<!-- $url -->\n");
    process_body($content, $retval);
    #warn Dumper($retval) if $i == 4;
    return undef if !$retval->{body};
    $retval->{body_link} = $url;

    $retval;
}

sub process_title {
    my ($xml, $out) = @_;
    my $data = XMLin($xml)->{channel}->{item};
    ($out->{category} = $data->{category}) =~ s/ >//;
    ($out->{title} = $data->{title}) =~ s/ >//;
    ($out->{date} = $data->{pubDate});
    #print Dumper($out);
}

sub process_body {
    my ($xml, $out) = @_;
    #warn $xml;
    my $data = XMLin($xml, ForceArray => ['item'])->{channel};
    #print Dumper($data);

    my $body = $data->{blog}->{blogcontent};
    if (!$body) {
        return undef;
    }
    $body =~ s/^\s+| >\s*$//sg;
    $out->{body} = $body;

    my $comments = $data->{item} || [];
    my @cmts;
    for my $comment (@$comments) {
        $comment = $comment->{comment};
        $comment->{pubDate} =~ s/^\s+|\s*$//sg;
        $comment->{commentnick} =~ s/^\s+| >\s*$//sg;
        $comment->{commentcontent} =~ s/^\s+| >\s*$//sg;
        my $cmt = {
            id       => $comment->{commentuin},
            name     => $comment->{commentnick},
            date     => $comment->{pubDate},
            content  => $comment->{commentcontent},
        };
        push @cmts, $cmt;
    }
    $out->{comments} = \@cmts;
    #print Dumper($out);
}

sub get_url {
    my ($agent, $url) = @_;
    for (1..3) {
        warn "  info: retrying $url...\n" if $_ > 1;
        $agent->get($url);
        if (!$agent->success) {
            warn "warning: $url: ", $agent->status, "\n";
        } elsif ($agent->content =~ m[<error>.*服务器\S+忙.*</error>]) {
            warn "warning: server too busy.\n";
        } else { last; }
    }
    #warn $agent->content;
    $agent->content;
}

1;
