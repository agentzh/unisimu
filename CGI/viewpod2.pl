use warnings;
use strict;
use HTTP::Server::Simple;

my $server = MyServer->new();
$server->run();

package MyServer;
use warnings;
use strict;
use Config;
use Pod::Html;

use base qw(HTTP::Server::Simple::CGI);

my $old_uri = '';

sub handle_request {
    my ($self, $cgi) = @_;     #... do something, print output to default
    # selected filehandle...
    my $fh;
    my $base = $cgi->url;
    $base = quotemeta($base);
    #warn "base = $base\n";
    my $file = $cgi->self_url;
    $file =~ s/^$base//;
    my $uri = $file;
    warn "file = $file\n";
    $file =~ s,^$old_uri,/,;
    $old_uri = $uri;
    $old_uri =~ s,[^/]+$,,;
    if ($file eq '/') {
        print(<<_EOC_);
<html>
<body>
<B>
<font color="red">
Please specify the name for a module, a script, or a POD file
in the URL.
</B>
</font>
<p>
<B>Examples:</B>
<p>
<pre>
    http://localhost:8080/Makefile::Parser
    http://localhost:8080/gvmake
    http://localhost:8080/CGI
</pre>
</body>
</html>
_EOC_
        return;
    }
    $file =~ s/\.html?$//;
    #$file .= '.pm' unless $file =~ m/\.pm$/i;
    $file =~ s,::,/,g;
    $file =~ s,\\,/,g;
    $file =~ s,-,/,g;
    $file =~ s,//+,/,g;
    warn "File name: $file\n";
    foreach ('.', 'lib', $Config{installsitebin},
             $Config{archlib}, $Config{installsitelib}) {
        for my $ext ('.pod','','.pm','.pl','.bat') {
            my $temp = $_.$file.$ext;
            #warn "  Trying ext $ext...\n";
            if (-f $temp) {
                $file = $temp;
                last;
            }
        }
    }
    if (!-f $file) {
        print header(-status=>$cgi->cgi_error);
        next;
    }
    pod2html($file,
        "--podroot=$Config{installsitebin}",
        "--htmlroot=.",
        "--recurse",
        "--infile=$file",
        "--outfile=$ENV{TEMP}/tmp.html",
        "--css=$Config{installhtmldir}/Active.css",
        "--header",
    );
    if (!open $fh, "$ENV{TEMP}/tmp.html") {
        print $cgi->header(-status=>$cgi->cgi_error);
        next;
    }
    clean_tmp();
    undef $/;
	print "HTTP/1.0 200 OK\r\n";
	print $cgi->header(-type=>'text/html');
    print <$fh>;
    close $fh;
}

sub clean_tmp {
    my ($tmp1, $tmp2) = qw(pod2htmd.tmp pod2htmi.tmp);
    unlink $tmp1 if -f $tmp1;
    unlink $tmp2 if -f $tmp2;
}

1;
