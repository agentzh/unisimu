use Template::Extract;
use Data::Dumper;

my $obj = Template::Extract->new;
my $template = << '.';
[%- FOREACH record %]
<TR bgColor=[% ... %]>
<TD>[% IP %]</TD>
<TD>[% port %]</TD>
<TD>[% ... %]</TD>
<TD>China</TD>
<TD>[% ... %]</TD></TR>
[% END -%]
.

my $document = << '.';
<TR>
<TD><A href="proxy1.htm">???</A></TD>
<TD><A href="proxy2.htm">???</A></TD>
<TD><A href="proxy3.htm">???</A></TD>
<TD><A href="proxy4.htm">???</A></TD>
<TD><A href="proxy5.htm">???</A></TD>
<TD><A href="proxy6.htm">???</A></TD>
<TD><A href="proxy7.htm">???</A></TD>
<TD><A href="proxy8.htm">???</A></TD>
<TD><A href="proxy9.htm">???</A></TD>
<TD><A href="proxy10.htm">???</A></TD></TR></TBODY></TABLE><A href="http://sh
op.7cv.com/index.php?asstfrom=wcr_wcr"><IMG alt="???? ?? ?? ?? ?? ?
? ?? ?? ?? ?? ?? ?? ?? ???" src="http://image.7cv.com/associat
e/images/20040902/b7.gif" border=0></A>
<SCRIPT language=JavaScript1.1 src="http://smarttrade.allyes.com/main/adfshow?us
er=1|13507|163304&amp;db=smarttrade&amp;border=0&amp;local=yes&amp;js=ie"></SCRI
PT>

<SCRIPT language=JavaScript1.1 src="http://smarttrade.allyes.com/main/adfshow?us
er=1|13507|163300&amp;db=smarttrade&amp;border=0&amp;local=yes&amp;js=ie"></SCRI
PT>

<SCRIPT language=JavaScript1.1 src="http://smarttrade.allyes.com/main/adfshow?us
er=1|13507|163308&amp;db=smarttrade&amp;border=0&amp;local=yes&amp;js=ie"></SCRI
PT>
 <IFRAME marginWidth=0 marginHeight=0 src="/money/google_ad_real.asp" frameBorde
r=0 width=728 scrolling=no height=90></IFRAME><A href="http://smarttrade.allyes.
com/main/adfclick?user=1|13507|163304&amp;db=smarttrade&amp;log=on&amp;ip=211.65
.91.132&amp;bid=4417&amp;cid=0&amp;sid=0&amp;kv=&amp;exp1=-499839917&amp;exp2=84
27365276&amp;cache=0&amp;url=http://bt.edodo.net/ad_hoc.asp?userid=4902715&amp;a
dtype=163304&amp;user_url=www.smarttrade.cn" target=_blank><IMG height=60 alt=""
 src="http://stbanner.allyes.com/banner/elong_mms_468x60.gif" width=468 border=0
></A><A href="http://smarttrade.allyes.com/main/adfclick?user=1|13507|163300&amp
;db=smarttrade&amp;log=on&amp;ip=211.65.91.132&amp;bid=4311&amp;cid=0&amp;sid=0&
amp;kv=&amp;exp1=-293869010&amp;exp2=8129345070&amp;cache=0&amp;url=http://www.9
6333.com/actionLog/actionLog.jsps?channelId=163300&amp;sid=4311&amp;from=smarttr
ade&amp;toUrl=/" target=_blank><IMG height=90 alt="" src="http://stbanner.allyes
.com/banner/sino_720+30b.gif" width=728 border=0></A>
<SCRIPT language=Javascript>
ADFHOST3311="http://smarttrade.allyes.com";
ADFCID3311=163308;
ADFLOC=\'CNJSZJ\';
ADFUSER3311="http://smarttrade.allyes.com/main/adfclick?user=1|13507|163308&db=s
marttrade&log=on&ip=211.65.91.132&bid=3311&cid=0&sid=0&exp1=-691869318&exp2=8027
355713&cache=632007&url=http://mlink.counter.dudu.com:8080/audit?a=10&b=163308&c
=1240&d=1477&e=2&g=allyes";
</SCRIPT>

<SCRIPT language=JavaScript>
<!--
var hposition20050705182914, vposition20050705182914;
var suijishu = 20050705182914;
var postbanner = 3311;
var flag20050705182914 ="0";
var wid20050705182914="300";
var hei20050705182914="300";
var scrollbars20050705182914=0;
var flash_adr20050705182914="http://stbanner.allyes.com/stbanner/420button_300_3
00.GIF";
var poptop_left20050705182914="Vleft";
var poptop_hei20050705182914="Vtop";
//-->
</SCRIPT>

<SCRIPT language=JavaScript1.1 src="http://stbanner.allyes.com/banner/popnew.js"
></SCRIPT>

<SCRIPT language=JavaScript>
hposition20050705182914 = 5;
</SCRIPT>

<SCRIPT language=JavaScript>
vposition20050705182914 = 5;
</SCRIPT>

<SCRIPT language=JavaScript>
flash_fl20050705182914=1;
var strwin20050705182914="toolbar=0,location=0,directories=0,status=0,menubar=0,
scrollbars="+scrollbars20050705182914+",resizable=1,copyhistory=0,width="+wid200
50705182914+",height="+hei20050705182914+",left=" + hposition20050705182914  + "
,top=" + vposition20050705182914;
</SCRIPT>

<SCRIPT language=JavaScript>
var win20050705182914=open("","abc20050705182914",strwin20050705182914);
</SCRIPT>
 ????????:2005-12-8 8:03:35<BR>
<TABLE cellSpacing=1 cellPadding=0 width="100%" bgColor=#a7b1bd border=0>
<TBODY>
<TR bgColor=#f9e6b3>
<TD width="30%">ip??</TD>
<TD width="15%">??</TD>
<TD width="15%">??</TD>
<TD width="20%">??</TD>
<TD width="20%">????</TD>
<TR>
<TR bgColor=#e3e6ea>
<TD>222.208.183.110</TD>
<TD>80</TD>
<TD>transparent</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>219.144.196.230</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.189.240.196</TD>
<TD>8080</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.54</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.103</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>222.66.5.205</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>211.167.4.106</TD>
<TD>80</TD>
<TD>high anonymity</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.117</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.89</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.122</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.90</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>221.10.55.226</TD>
<TD>8080</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>218.22.246.34</TD>
<TD>80</TD>
<TD>high anonymity</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.51</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.52</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>221.212.177.97</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.125</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.79</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>202.108.119.227</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.119</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.85</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.81</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.96</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>221.10.124.34</TD>
<TD>8080</TD>
<TD>high anonymity</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.50</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.129</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.112</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.118</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR>
<TR bgColor=#e3e6ea>
<TD>61.135.158.128</TD>
<TD>80</TD>
<TD>anonymous</TD>
<TD>China</TD>
<TD>2005-12-8</TD></TR></TBODY></TABLE>
<SCRIPT language=VBScript src="ad.js" type=text/VBScript></SCRIPT>
<A href="http://shop.7cv.com/index.php?asstfrom=wcr_wcr"><IMG alt="???? ??
 ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ???" src="http://imag
e.7cv.com/associate/images/20040902/b2.gif" border=0></A>
<SCRIPT language=VBScript src="bottom.js" type=text/VBScript></SCRIPT>
 <IFRAME id=baiduframe border=0 marginWidth=0 frameSpacing=0 marginHeight=0 src=
"http://unstat.baidu.com/bdun.bsc?tn=wcrproxy&amp;csid=107&amp;rkcs=2&amp;bgcr=F
FFFFF&amp;ftcr=000000&amp;rk=1&amp;bd=1&amp;bdas=0" frameBorder=0 width=468 scro
lling=no height=40>
</IFRAME></BODY></HTML>
.

print Data::Dumper::Dumper(
    $obj->extract($template, $document)
);
