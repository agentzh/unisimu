#: plotqzone.pl
#: 2006-07-11 2006-07-11

use Encode 'from_to';
use Template;
use Getopt::Std;
use YAML::Syck;

$Template::Stash::SCALAR_OPS->{ resolve_meta } = sub {
    my $s = shift;

    # process image meta tags:
    $s =~ s,\[img\] ([^\[]*) \[/img\],
        <a href="$1"><img src="$1" alt="$1" width=50 /></a>,xsg;
    $s =~ s,\[em\] ([^\[]*) \[/em\],<img src="http://imgcache.qq.com/qzone/em/$1.gif" />,xsg;

    # process font color meta tags:
    $s =~ s,\[ ftc \&\#61; ([^\]]*) \] ([^\[]*) \[/ft\],
        <font color="$1">$2</font>,gxsi;

    # process font name meta tags:
    $s =~ s,\[ ftf \&\#61; ([^\]]*) \] ([^\[]*) \[/ft\],
        <font face="$1">$2</font>,gxsi;

    # process font size meta tags:
    my $size;
    $s =~ s,\[ fts \&\#61; ([^\]]*) \] ([^\[]*) \[/ft\],
        $size=$1;$size=6 if $size > 6;"<font size=\"$size\">$2</font>",gxsie;

    # process center alignment meta tags:
    $s =~ s,\[M\] ([^\[]*) \[/M\],<center>$1</center>,gxsi;

    $s =~ s,\[ ff[a-z] \&\#61; [^\]]* \],,gxsi;

    # support for [url][/url]
    $s =~ s,\[url \&\#61; ([^\]]*)\] ([^\[]*) \[/url\],<a href="$1">$2</a>,gxsi;

    # support for [B][/B]
    $s =~ s,\[B\] ([^\[]*) \[/B\],<B>$1</B>,gxsi;

    # support for [I][/I]
    $s =~ s,\[I\] ([^\[]*) \[/I\],<I>$1</I>,gxsi;

    # support for [U][/U]
    $s =~ s,\[U\] ([^\[]*) \[/U\],<U>$1</U>,gxsi;

    # XXX support for [flash][/flash]

    $s =~ s,\&\#13;\&\#10;,<p />,gxs;
    $s =~ s,\&\#10;,<p />,gxs;
    $s =~ s,\&\#32;,\&nbsp;,gxs;
    $s;
};

$Template::Stash::SCALAR_OPS->{ strtime } = sub {
    my $time = shift;
    #warn $time;
    my $s = scalar localtime $time;
    #warn $s;
    $s;
};

$Template::Stash::SCALAR_OPS->{ gb2utf } = sub {
    my $s = shift;
    from_to($s, "gb2312", "utf8");
    $s;
};

my $qq_number = shift;
if (!$qq_number || $qq_number !~ /^\d+$/) {
    die "Usage: $0 [-d] <qq-number>";
}

my $infile  = "$qq_number.yml";
my $outfile = "$qq_number.html";

my $ast = LoadFile($infile);
if (!$ast) {
    die "error: no data loaded from $infile.\n";
}


my ($sec, $min, $hour, $mday, $mon, $year) = gmtime;
$year += 1900; $mon += 1;
my $time = sprintf "%04d-%02d-%02d %02d:%02d:%02d GMT",
    $year, $mon, $mday, $hour, $min, $sec;
$ast->{timestamp} = $time;

my $tt = Template->new;

$tt->process(\*DATA, $ast, $outfile)
    or die $tt->error(), "\n";
warn "$outfile generated.\n";

__DATA__
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

[%- s = '空间文章汇总' %]
[%- title = personal_info.nickname _ " - QZone " _ s.gb2utf -%]

<title>[% title %]</title>
<link rel="stylesheet" href="qzone.css" type="text/css" />
</head>

<body>

<table border="0" width="100%" cellspacing="0" cellpadding="3">
<tr><td class="block" valign="middle">
<big><strong><span class="block">&nbsp;[% title %]</span></strong></big>
</td></tr>
</table>
<p />

<p><a id="__index__"></a></p>
<!-- INDEX BEGIN -->

<ul>
[%- s = '文档生成日期';
    h0 = s.gb2utf;
    s = '个人信息';
    h1 = s.gb2utf;
    s = '留言板';
    h2 = s.gb2utf;
    s = '所有文章';
    h3 = s.gb2utf
%]
    <li><a href="#h0">[% h0 %]</a></li>
    <li><a href="#h1">[% h1 %]</a></li>
    <li><a href="#h2">[% h2 %]</a></li>
    <li><a href="#h3">[% h3 %]</a></li>
[%- i = 1 %]
[%- FOREACH category = blogs.keys.sort %]
  [%- group = blogs.$category %]
    <ul>
      <li><a href="#cat_[% i %]">[% category %]</a></li>
      <ul>
      [%- j = 1 %]
      [%- FOREACH blog = group %]
          <li><a href="#blog_[% i %]_[% j %]">[% blog.title %]</a></li>
        [%- j = j + 1 %]
      [%- END %]
      </ul>
    </ul>
  [%- i = i + 1 %]
[%- END %]

</ul>

<!-- INDEX END -->

<p />
<hr />
<p />

[%- s = '文档生成日期' %]
<h1><a id="h0">[% s.gb2utf %]</a></h1>

&nbsp; &nbsp; &nbsp; [% timestamp %]

<p />
<hr />
<p />

<h1><a id="h1">[% h1 %]</a></h1>

<ul>
<table border=0 cellspacing=2 cellpadding=7>
<tr>
    [%- s = '昵称' %]
    <td width=120><B>QQ [% s.gb2utf %]</B></td>
    <td>[% personal_info.nickname %]</td>
</tr>
<tr>
    [%- s = '号码' %]
    <td><B>QQ [% s.gb2utf %]</B></td>
    <td>[% personal_info.qq_number %]</td>
</tr>
<tr>
    [%- s = '性别' %]
    <td><B>[% s.gb2utf %]</B></td>
    <td>[%- IF personal_info.sex == 'F';
              s = '女';
              GET s.gb2utf;
            ELSIF personal_info.sex == 'M';
              s = '男';
              GET s.gb2utf;
            END %]</td>
</tr>
<tr>
    [%- s = '年龄' %]
    <td><B>[% s.gb2utf %]</B></td>
    <td>[% personal_info.age %]</td>
</tr>
[%- IF personal_info.birthday %]
<tr>
    [%- s = '出生日期' %]
    <td><B>[%- s.gb2utf %]</B></td>
    <td>[% personal_info.birthday %]</td>
</tr>
[%- END %]
<tr>
    [%- s = '首页' %]
    <td><B>QZone [% s.gb2utf %]</B></td>
    <td><a target="_blank" href="[% personal_info.qzone_home %]">
    [%- personal_info.qzone_home %]</a></td>
</tr>
[%- IF personal_info.spacename %]
<tr>
    [%- s = '空间名称' %]
    <td><B>Qzone [% s.gb2utf %]</B></td>
    <td>[% personal_info.spacename %]</td>
</tr>
[%- END %]

</table>
</ul>

<p />
<hr />
<p />

<h1><a id="h2">[% h2 %]</a></h1>

[%- FOREACH msg = msg_board %]
<ul>
    <li>
    <b>[% msg.name %]</b>
    [%- s = 'uin=' _ msg.id %]
    <a target="_blank"
       href="[% personal_info.qzone_home.replace('uin=\d+', s) %]">([% msg.id %])</a>
    &nbsp; - &nbsp;[%- msg.date %]
    <p />
    [%- msg.body.resolve_meta %]
    </li>
    <br />
    <br />
</ul>
[%- END %]

<p />
<hr />
<p />

<h1><a id="h3">[% h3 %]</a></h1>

[%- i = 1 %]
[%- FOREACH category = blogs.keys.sort %]
  [%- group = blogs.$category %]
    <h2><a id="cat_[% i %]">[% category %]</a></h2>
      [%- j = 1 %]
      [%- FOREACH blog = group %]
          <h3><a id="blog_[% i %]_[% j %]">[% blog.title %]</a></h3>

            <p><I>[% blog.date.strtime %]</I></p>

            <div>
              [%- blog.body.resolve_meta %]
            </div>
         [%- IF blog.comments.size %]
            <h4> Comments </h4>
            [%- FOREACH comment = blog.comments %]
              <ul>
              <li>
              <b>[% comment.name %]</b>
                 [%- s = 'uin=' _ comment.id %]
                 <a target="_blank"
                 href="[%- personal_info.qzone_home.replace('uin=\d+', s) %]">
                   ([% comment.id %])
                 </a>
              &nbsp; - &nbsp;[%- comment.date %]
              <p />
              [%- comment.content.resolve_meta %]
              </li>
            <br />
            <br />
            </ul>
            [%- END %]
         [%- END %]

        [%- j = j + 1 %]
      [%- END %]
      </ul>
    </ul>
  [%- i = i + 1 %]
[%- END %]

<table border="0" width="100%" cellspacing="0" cellpadding="3">
<tr><td class="block" valign="middle">
<big><strong><span class="block">&nbsp;[% title %]</span></strong></big>
</td></tr>
</table>

</body>

</html>
