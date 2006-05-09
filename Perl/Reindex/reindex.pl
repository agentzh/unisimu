#: reindex.pl
#: reindex .t files for Test::Base based test files
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-27 2006-05-09

use strict;
use warnings;

#use File::Copy;
use Getopt::Std;

my %opts;
getopts('hb:', \%opts);
if ($opts{h} or ! @ARGV) {
    die "Usage: reindex [-b 0] t/*.t\n";
}

my $init = $opts{b};
$init = 1 if not defined $init;

my @files = map glob, @ARGV;
for my $file (@files) {
    next if -d $file or $file !~ /\.t$/;
    reindex($file);
}

sub reindex {
    my $file = $_[0];
    open my $in, $file or
        die "Can't open $file for reading: $!";
    my @lines;
    my $counter = $init;
    my $changed;
    while (<$in>) {
        my $num;
        s/^=== \s+ TEST \s+ (\d+)/$num=$1; "=== TEST " . $counter++/xe;
        next if !defined $num;
        if ($num != $counter-1) {
            $changed++;
        }
    } continue {
        push @lines, $_;
    }
    close $in;
    if (! $changed) {
        warn "reindex: $file:\tskipped.\n";
        return;
    }
    #File::Copy::copy( $file, "$file.bak" );
    open my $out, "> $file" or
        die "Can't open $file for writing: $!";
    binmode $out;
    print $out @lines;
    close $out;
    
    warn "reindex: $file:\tdone.\n";
}
