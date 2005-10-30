@rem = '--*-Perl-*--
@echo off
if "%OS%" == "Windows_NT" goto WinNT
perl -x -S "%0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:WinNT
perl -x -S %0 %*
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofperl
if %errorlevel% == 9009 echo You do not have Perl in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofperl
@rem ';
#!perl
#line 15
use strict;
use warnings;
use HTTP::Server::Simple;

my $server = MyServer->new();
$server->run();

package MyServer;

use strict;
use warnings;
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
    warn "File name: $file\n";
    $file =~ s,^$old_uri,/, if $old_uri;
    $old_uri = $uri;
    $old_uri =~ s,[^/]+$,,;

    $file =~ s,//+,/,g;
    #warn "File now is $file!\n";
    if ($file eq '/' or $file eq '/home') {
        #warn "Here!!!!\n";
        $file = '/modlist';
    }

    if ($file =~ m,/modlist,i) {
        if (!update_modlist($cgi)) {
            return 0;
        }
    }
            
    $file =~ s/\.html?$//;
    #$file .= '.pm' unless $file =~ m/\.pm$/i;
    $file =~ s,::,/,g;
    $file =~ s,\\,/,g;
    $file =~ s,-,/,g;
    $file =~ s,//+,/,g;
    warn "  File Name: $file\n";
    foreach ('.', 'lib', $Config{installsitebin},
             $Config{archlib}, $Config{installsitelib},
             "$Config{archlib}/Pod") {
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
        error(
            $cgi,
            "File $file not found!");
        return;
    }
    pod2html($file,
        "--podroot=$Config{installsitebin}",
        "--podroot=$Config{archlib}",
        "--podroot=$Config{archlib}/Pod",
        "--htmlroot=.",
        "--recurse",
        "--infile=$file",
        "--outfile=$ENV{TEMP}/tmp.html",
        "--css=$Config{installhtmldir}/Active.css",
        "--header",
    );
    if (!open $fh, "$ENV{TEMP}/tmp.html") {
        error(
            $cgi,
            "Can't open $ENV{TEMP}/tmp.html for reading: $!");
        return;
    }
    clean_tmp();
    undef $/;
	print "HTTP/1.0 200 OK\r\n";
	print $cgi->header(-type=>'text/html', -target=>'content');
    print <$fh>;
    close $fh;
}

sub clean_tmp {
    my ($tmp1, $tmp2) = qw(pod2htmd.tmp pod2htmi.tmp);
    unlink $tmp1 if -f $tmp1;
    unlink $tmp2 if -f $tmp2;
}

sub error {
    my ($cgi, $str) = @_;
    print "HTTP/1.0 200 OK\r\n";
    print $cgi->header(-type=>'text/html');
    print $cgi->start_html('Server Error');
    print $cgi->h1("Server Error: $str");
    print $cgi->end_html;
}

sub update_modlist {
    my $cgi = shift;
    my ($infile, $outfile) = (
        "$Config{archlib}/perllocal.pod",
        "$Config{archlib}/modlist.pod",
    );
    my %modules;
    my $in;
    if (!open $in, $infile) {
        error(
            $cgi,
            "Can't open $infile for reading: $!");
        return 0;
    }
    my $name;
    while (<$in>) {
        if (/^=head2 (.+) C<Module> L<(.+)>/i) {
            $name = $2;
            if (!$modules{$name}) {
                $modules{$name} = {
                    date => $1,
                    ver => 'I<unknown>',
                };
            }
        }
        elsif (/^C<VERSION:\s*(\S+)>/i) {
            next if !$name and !$modules{$name};
            $modules{$name}->{ver} = $1;
        }
    }
    close $in;

    #warn "Opening $outfile for writing...\n";
    my $out;
    if (!open $out, ">$outfile") {
        error(
            $cgi,
            "Can't open $outfile for writing: $!");
        return 0;
    }
    print $out "=head1 NAME\n\nInstalled Perl Module List\n\n";
    foreach (sort keys %modules) {
        my $val = $modules{$_};
        my ($date, $ver) = ($val->{date}, $val->{ver});
        print $out "=head1 L<$_>\n\n".
                   "=over\n\n".
                   "=item *\n\n".
                   "C<VERSION: $ver>\n\n".
                   "=item *\n\n".
                   "C<INSTALLATION DATE: $date>\n\n".
                   "=back\n\n";
    }
    close $out;
    return 1;
}

1;

__END__
:endofperl
