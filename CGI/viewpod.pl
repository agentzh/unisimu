use strict;
use warnings;

use HTTP::Daemon;
use HTTP::Status;
use Pod::Html;
use Config;

my $file;
#die "File $file not found" unless -e $file;

my $fh;
my $old_uri = '';
my $d = HTTP::Daemon->new || die;
#system("explorer ".$d->url."\n");
print "Please contact me at: <URL:", $d->url, ">\n";
while (my $c = $d->accept) {
    while (my $r = $c->get_request) {
        if ($r->method eq 'GET') {
            my $rs = new HTTP::Response(RC_OK);
            #warn $r->uri;
            $file = $r->uri;
            $file =~ s,^$old_uri,/,;
            $old_uri = $r->uri;
            $old_uri =~ s,[^/]+$,,;
            if ($file eq '/') {
                $rs->content(<<_EOC_);
<html>
<body>
<B>
<font color="red">
Please specify a name for a module, a script, or a POD file
in the URL.
</B>
</font>
<p>
<B>Examples:</B>
<p>
<pre>
    http://agent:1093/Makefile::Parser
    http://agent:1093/gvmake
    http://agent:1093/CGI
</pre>
where the local addresss 'http://agent:1093/' should be replaced
by the URL offered by B<viewpod>.
</body>
</html>
_EOC_
                $c->send_response($rs);
                next;
            }
            $file =~ s/\.html?$//;
            #$file .= '.pm' unless $file =~ m/\.pm$/i;
            $file =~ s,::,/,g;
            $file =~ s,\\,/,g;
            $file =~ s,-,/,g;
            warn "File name: $file\n";
            foreach ('.', 'lib', $Config{installsitebin},
                     $Config{archlib}, $Config{installsitelib}) {
                for my $ext ('.pod','','.pm','.pl','.bat',) {
                    my $temp = $_.$file.$ext;
                    #warn "  Trying ext $ext...\n";
                    if (-f $temp) {
                        $file = $temp;
                        last;
                    }
                }
            }
            if (!-f $file) {
                $c->send_error(RC_FORBIDDEN);
            }
            pod2html($file,
                "--podroot=$Config{installsitebin}",
                "--htmlroot=.",
                "--recurse",
                "--infile=$file",
                "--outfile=tmp.html",
                "--css=d:/perl/docstyle.css",
                "--header",
            );
            if (!open $fh, "tmp.html") {
                $c->send_error(RC_FORBIDDEN);
                next;
            }
            undef $/;
            $rs->content(<$fh>);
            close $fh;
            $c->send_response($rs);
        } else {
            $c->send_error(RC_FORBIDDEN);
        }
    }
    $c->close;
    undef($c);
}
