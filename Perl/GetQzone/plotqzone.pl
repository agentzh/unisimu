#: plotqzone.pl
#: 2006-07-11 2006-07-11

use Encode 'from_to';
use Template;
use Getopt::Std;
use YAML::Syck;

$Template::Stash::SCALAR_OPS->{ resolve_meta } = sub {
    my $s = shift;
    $s =~ s,\[img\] ([^\[]*) \[/img\],
        <a href="$1"><img src="$1" alt="$1" width=50 /></a>,xsg;
    $s =~ s,\[em\] ([^\[]*) \[/em\],<img src="http://imgcache.qq.com/qzone/em/$1.gif" />,xsg;

    $s =~ s,\[ ft[a-z] \&\#61; [^\]]* \],,gxs;
    $s =~ s,\[ /ft \],,gxs;

    $s =~ s,\[B\],<B>,gxs;
    $s =~ s,\[/B\],</B>,gxs;

    $s =~ s,\[I\],<I>,gxs;
    $s =~ s,\[/I\],</I>,gxs;

    # XXX support for [u][/u]
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

my $s1 = '空间文章汇总';
from_to($s1, "gb2312", "utf8");

my $h1 = '个人信息';
from_to($h1, 'gb2312', 'utf8');

my $h2 = '留言板';
from_to($h2, 'gb2312', 'utf8');

my $h3 = '所有文章';
from_to($h3, 'gb2312', 'utf8');

my $data = {
    ast   => $ast,
    title => $ast->{personal_info}->{nickname} . " - " . " QZone $s1",
    h1    => $h1,
    h2    => $h2,
    h3    => $h3,
};
my $tt = Template->new;

$tt->process(\*DATA, $data, $outfile)
    or die $tt->error(), "\n";
warn "$outfile generated.\n";

__DATA__
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>[% title %]</title>
<link rel="stylesheet" href="Active.css" type="text/css" />
</head>

<body>

<table border="0" width="100%" cellspacing="0" cellpadding="3">
<tr><td class="block" valign="middle">
<big><strong><span class="block">&nbsp;[% title %]</span></strong></big>
</td></tr>
</table>
<p />

<p><a name="__index__"></a></p>
<!-- INDEX BEGIN -->

<ul>
    <li><a href="#h1">[% h1 %]</a></li>
    <li><a href="#h2">[% h2 %]</a></li>
    <li><a href="#h3">[% h3 %]</a></li>
[%- i = 1 %]
[%- FOREACH category = ast.blogs.keys.sort %]
  [%- blogs = ast.blogs.$category %]
    <ul>
      <li><a href="#cat_[% i %]">[% category %]</a></li>
      <ul>
      [%- j = 1 %]
      [%- FOREACH blog = blogs %]
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
<hr>
<p />
<h1><a name="h1">[% h1 %]</a></h1>

[%- data = ast.personal_info %]
<ul>
<table border=0 cellspacing=2 cellpadding=6>
<tr>
    <td><B>Nick Name</B></td>
    <td>[% data.nickname %]</td>
</tr>
<tr>
    <td><B>QQ Number</B></td>
    <td>[% data.qq_number %]</td>
</tr>
<tr>
    <td><B>Sex</B></td>
    <td>[%- IF data.sex == 'F' -%]
            Female
        [%- ELSIF data.sex == 'M' -%]
            Male
        [%- ELSE -%]
            <I>Unknow</I>
        [%- END %]</td>
</tr>
<tr>
    <td><B>Age</B></td>
    <td>[% data.age %]</td>
</tr>
[%- IF data.birthday %]
<tr>
    <td><B>Birthday</B></td>
    <td>[% data.birthday %]</td>
</tr>
[%- END %]
<tr>
    <td><B>QZone Home</B></td>
    <td><a target="blank" href="[% data.qzone_home %]">[% data.qzone_home %]</a></td>
</tr>
[%- IF data.spacename %]
<tr>
    <td><B>Qzone Space Name</B></td>
    <td>[% data.spacename %]</td>
</tr>
[%- END %]

</table>
</ul>

<p />
<hr>
<p />
<h1><a name="h2">[% h2 %]</a></h1>

[%- FOREACH msg = ast.msg_board %]
<ul>
    <li></li>
    <b>[% msg.name %]</b>
    [%- s = 'uin=' _ msg.id %]
    <a target="blank" href="[%- data.qzone_home.replace('uin=\d+', s) %]">([% msg.id %])</a>
    &nbsp; - &nbsp;[%- msg.date %]
    <p />
    [%- msg.body.resolve_meta %]
    <br />
    <br />
</ul>
[%- END %]

<p />
<hr>
<p />
<h1><a name="h3">[% h3 %]</a></h1>

[%- i = 1 %]
[%- FOREACH category = ast.blogs.keys.sort %]
  [%- blogs = ast.blogs.$category %]
    <h2><a name="#cat_[% i %]">[% category %]</a></h2>
      [%- j = 1 %]
      [%- FOREACH blog = blogs %]
          <h3><a name="#blog_[% i %]_[% j %]">[% blog.title %]</a></h3>

            <p><I>[% blog.date.strtime %]</I></p>

            <div>
              [%- blog.body.resolve_meta %]
            </div>
         [%- IF blog.comments.size %]
            <h4> Comments </h4>
            [%- FOREACH comment = blog.comments %]
              <ul>
              <li></li>
              <b>[% comment.name %]</b>
                 [%- s = 'uin=' _ comment.id %]
                 <a target="blank" href="[%- data.qzone_home.replace('uin=\d+', s) %]">
                   ([% comment.id %])
                 </a>
              &nbsp; - &nbsp;[%- comment.date %]
              <p />
              [%- comment.content.resolve_meta %]
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

<p />
<hr>
<p />

<table border="0" width="100%" cellspacing="0" cellpadding="3">
<tr><td class="block" valign="middle">
<big><strong><span class="block">&nbsp;[% title %]</span></strong></big>
</td></tr>
</table>

</body>

</html>
