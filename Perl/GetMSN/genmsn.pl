use strict;
use warnings;

use Encode 'from_to';
use Template;
use Getopt::Std;
use YAML::Syck;

my %opts;
getopts('c', \%opts);

my $name = shift or die "Usage: genmsn [-c] <username>";
$name =~ s/\.yml$//;
my $journals = LoadFile("$name.yml");
if (!$journals) {
    die "error: no data loaded from $name.yml.\n";
}
for my $journal (@$journals) {
    my $body = $journal->{body};
    $body =~ s,</table>|</td>|</tr>,,gsi;
    $body =~ s,<table\b|<td\b|<tr\b,<div,gsi;
    $body =~ s, style=", id=",gs;
    $journal->{body} = $body;
}
my $s = '空间网络日志汇总';
from_to($s, "gb2312", "utf8");
my $data = {
    journals => $journals,
    user     => $name,
    title    => "$name - MSN $s",
    cmt_on   => $opts{c},
};
my $tt = Template->new;
$tt->process(\*DATA, $data, "$name.html")
    or die $tt->error(), "\n";
warn "$name.html generated.\n";

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

[%- i = 1 %]
[%- FOREACH journals %]
    <li><a href="#title_[% i %]">[% title %]</a></li>
    <ul>
      <li>[% date %] [% time %]</li>
    </ul>
  [%- i = i + 1 %]
[%- END %]

</ul>

<!-- INDEX END -->

[%- i = 1 %]
[%- FOREACH journals %]
<p />
<hr>
<p />
<h1><a name="title_[% i %]">[% title %]</a></h1>
  <h2>[% date %] [% time %]</h2>
<div>
  [%- body %]
<p />
&nbsp;&nbsp;<small>Link: <a href="[% link %]">[% link %]</a></small>
<p />
</div>
  [%- IF cmt_on %]
    [%- FOREACH comment = comments %]
    <ul>
    <li>[% comment.author %]</li>
    [% comment.body %]
    </ul>
    [%- END %]
  [%- END %]
  [%- i = i + 1 %]
[%- END %]

<p />
<p />

<table border="0" width="100%" cellspacing="0" cellpadding="3">
<tr><td class="block" valign="middle">
<big><strong><span class="block">&nbsp;[% title %]</span></strong></big>
</td></tr>
</table>

</body>

</html>
