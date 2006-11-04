use strict;
use Test::More tests => 3;
use Template::Plugin::Perl;

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use Template;
use File::Compare 'compare_text';

my $dir = '.';
$dir = 't' if -d 't';
my $ttfile = "$dir/test.tt";
my $outfile = "$dir/out";

my $tt = Template->new;
ok($tt->process($ttfile, {}, $outfile));
ok(-f $outfile);
is(compare_text($outfile,"$outfile~"), 0);
