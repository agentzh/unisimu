#: getsvn.pl
#: mirror SVN repos via plain HTTP protocol
#: Copyright (c) 2006 Agent Zhang
#: 2006-4-2 2006-4-2

use strict;
use warnings;

use Getopt::Std;
#use Data::Dumper::Simple;
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
    if (not $mech->success and not retry($url, 3, $indent)) {
        return;
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
    my $res = $mech->get( $fname );
    if (not $mech->success and not retry($fname, 3)) {
        return;
    }
    my $indent = '  ' x $level;
    warn "${indent}Saving $fname to $dir...\n";
    my $path = "$dir/$fname";
    open my $out, "> $path" or
        die "Can't open $path for writing: $!";
    binmode $out;
    print $out $res->content();
    close $out;
    $mech->back;
}

sub retry {
    my ($url, $max, $indent) = @_;
    for (1..3) {
        warn "${indent}Reloading $url...\n";
        $mech->reload();
        return 1 if $mech->success;
        if ($_ == 3) {
            warn "Warning: Can't get $url: ", $mech->status(), "\n";
            return undef;
        }
    }
}
