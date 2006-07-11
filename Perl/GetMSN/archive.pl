#: archive.pl
#: 2006-07-11 2006-07-11

use strict;
use warnings;

use File::Copy 'cp';
use File::Spec;
use Date::Simple 'today';

my @patterns = ('*.html', '*.dat', '*.yml', '*.css');

my $dir = 'Archives';
if (!-d $dir) { mkdir($dir); }

$dir = "$dir/" . today();
if (!-d $dir) { mkdir($dir); }

warn "info: $dir\n";

my @files = map glob, @patterns;
for my $file (@files) {
    my $file2 = File::Spec->catfile($dir, $file);
    warn "  info: archiving $file to $dir...\n";
    cp($file, $file2) or
        warn "warning: fail to copy $file to $file2: $!\n";
}
