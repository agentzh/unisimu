#: get-proxy.pl
#: get proxy list from the web
#: Copyright (c) 2005 Agent Zhang
#: 2005-12-08 2005-12-16

use strict;
use warnings;

#use Smart::Comments;
use Template::Extract;
use Win32::IE::Mechanize;
use Getopt::Std;

my %opts;
getopts('ho:', \%opts);

if ($opts{h}) {
    print <<_EOC_;
usage: get-proxy.pl <config-file>
_EOC_
    exit(0);
}

my $infile = shift or
    die "No input file specified.\nUse -h for more info.\n";
my $outfile = $opts{o} or
    die "No output file specified.\nUse -h for more info.\n";

open my $in, $infile or
    die "Can't open $infile for reading: $!\n";

my $in_tpl = 0;
my ($url, $tpl, %src);
print "Parsing $infile...\n";
while (<$in>) {  ### Working===[%]     done
    if (m[^(https?://.+)$]) {
        if ($in_tpl and $url and $tpl) {
            $src{$url} = $tpl;
        }
        $url = $1;
        $tpl = '';
        $in_tpl = 1;
    } elsif ($in_tpl) {
        $tpl .= $_;
    }
}
close $in;

if ($in_tpl and $url and $tpl) {
    $src{$url} = $tpl;
}

open my $out, ">$outfile" or
    die "Can't open $outfile for writing: $!\n";

my $ie = Win32::IE::Mechanize->new( visible => 1 );
my $extract = Template::Extract->new;

my @records;
while (my ($url, $tpl) = each %src) {
    print "Getting $url...\n";
    $ie->get($url);
    print "Press Enter to continue";
    <STDIN>;
    #if (not $ie->success or not $ie->is_html) {
    #    sleep(1);
    #    warn "Failed to get '$url'\n";
    #}
    my $html = $ie->content;
    $html =~ s/\r\n/\n/g;
    print $html, "\n";
    my $ast = $extract->extract($tpl, $html);
    ### $tpl
    ### $ast
    push @records, @{$ast->{record}};
}

foreach my $r (@records) {
    print $out ($r->{IP}, ':', $r->{port}, "\n");
}
close $out;

print "\nFor total ", scalar(@records), " proxy IP(s) obtained.\n";

__END__

This proxy is awfully fast: 202.189.126.86:3124
But it can't connect to a SSL site and it also counts the requests on your IP.
