#: getsvn.pl
#: mirror SVN repos via plain HTTP protocol
#: Copyright (c) 2006 Agent Zhang
#: 2006-4-2 2006-4-2

use strict;
use warnings;

use Getopt::Std;
use Data::Dumper::Simple;
use WWW::Mechanize;

my %opts;
getopts('h', \%opts);

if ($opts{h}) {
    Usage();
}

my ($url, $dir) = @ARGV;
if (!defined $url) {
    warn "error: No base URL given\n\n";
    Usage();
}
if (!defined $dir) {
    warn "error: No directory given\n\n";
    Usage();
}

sub Usage {
    print <<_EOC_;
Usage: getsvn http://url.to.svn.repos local/directory

Options:
    -h    Print this message to stdout
_EOC_
    exit(0);
}

my $mech = WWW::Mechanize->new();
get_repos($url, $dir);

sub get_repos {
    my ($url, $dir, $level) = @_;
    $level ||= 1;
    my $indent = '  ' x $level;
    my @a = split /[\\\/]+/, $url;
    my $fname = pop (@a);
    warn "${indent}Fetching $url...\n";
    $mech->get( $url );
    for (1..3) {
        last if $mech->success;
        warn "${indent}Reloading $url...\n";
        $mech->reload();
        if ($_ == 3) {
            warn "Warning: Can't get $url: ", $mech->status(), "\n";
        }
    }
    #my $base = $mech->base;
    if (!-d $dir) {
        mkdir $dir;
    }
    for my $link ($mech->links) {
        my $sub_url = $link->url;
        #print Dumper($url);
        next if $sub_url eq '../' || $sub_url =~ m[^http://];
        if ($sub_url =~ m[/$]) {
            get_repos($sub_url, "$dir/$sub_url", $level+1);
        } else {
            get_file($sub_url, $dir, $level+1);
        }
    }
    $mech->back;
}

sub get_file {
    my ($fname, $dir, $level) = @_;
    my $indent = '  ' x $level;
    warn "${indent}Saving $fname to $dir...\n";
    $mech->save_content("$dir/$fname");
}
