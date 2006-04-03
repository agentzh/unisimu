#: getxul.pl
#: retrieve XUL files and their dependencies
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-03 2006-04-03

use strict;
use warnings;

use Getopt::Std;
use WWW::Mechanize;
use File::Basename;

my %opts;
getopts('d:', \%opts);

my $tar_dir = $opts{d} || '';
$tar_dir .= '/' if $tar_dir;

my $mech = WWW::Mechanize->new;

my $url = shift or die "No URL specified";
my $res = $mech->get($url);
if (! $mech->success) {
    die "Can't retrieve $url: ".$mech->status;
}

my ($fname) = ($url =~ /([^\/\\]+)$/);
save_file($res, $fname);

my $content = $mech->content;

while ($content =~ m/(?:src|href) \s* = \s* "([^"]+)"/xgi) {
    my $file = $1;
    next if ($file eq '#' or $file =~ m[chrome://]i);
    get_file($1);
}

sub save_file {
    my ($res, $fname) = @_;
    $fname = $tar_dir.$fname;
    my $dir = dirname($fname);
    if (!-d $dir) { mkdir $dir; }
    warn "Saving $fname...\n";
    open my $out, "> $fname" or
        die "Can't open $fname for writing: $!";
    binmode $out;
    print $out $res->content();
    close $out;
}

sub get_file {
    my $fname = shift;
    my $res = $mech->get($fname);
    if (! $mech->success) {
        warn "Can't get $fname: ".$mech->status;
    }
    save_file($res, $fname);
    $mech->back;
}
