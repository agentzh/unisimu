#: gen_t.pl
#: generate perl stub scripts for every .t.cpp in the cwd
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-10 2006-04-10

use strict;
use warnings;

my @files = glob "*.t.cpp";
die "No *.t.cpp found in the current directory"
    if ! @files;
map { s/\.cpp$//; } @files;
for my $file (@files) {
    open my $out, "> $file" or
        die "Can't open $file for writing: $!";
    print $out (<<_EOC_);
use FindBin;
system("\\"\$FindBin::Bin/$file.exe\\"");
_EOC_
    close $out;
    print "$file generated.\n";
}
