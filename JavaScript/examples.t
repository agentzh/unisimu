# examples.t
# Test the sample JS files using perljs.pl
# 2006-06-11 2006-06-11

use strict;
use warnings;

use Test::Base;
use IPC::Run3;

plan tests => 2 * blocks();

my @perljs = ($^X, 'perljs.pl');

run {
    my $block = shift;
    my $name = $block->name;
    my $jsfile = $block->args;
    $jsfile =~ s/^\s+|\s+$//gs;
    my @args = split(/\s+/, $jsfile);
    my @cmd = ( @perljs, @args );
    my ($in, $out, $err);
    $in = $block->stdin;
    run3 \@cmd, \$in, \$out, \$err;
    is $err, $block->stderr, "$name - stderr ok";
    is $out, $block->stdout, "$name - stdout ok";
};


__DATA__

=== TEST 1: test.js (say, dump, print, arguments, and etc.)
--- args
test.js
--- stderr
Yay!
--- stdout
hello, world!
undefined, undefined
abcd



=== TEST 1: test.js (say, dump, print, arguments, and etc.)
--- args
test.js abc 123
--- stderr
Yay!
--- stdout
hello, world!
abc, 123
abcd



=== TEST 2: test2.js w/o -Ilib and thus no Test.More
--- args
test2.js
--- stderr
Fail to find JS module Test/More.js (JS_INC contains ".")
--- stdout



=== TEST 3: test2.js with -Ilib and Test.More
--- args
-Ilib test2.js
--- stderr
--- stdout
1..6
ok 1
ok 2
ok 3
ok 4
ok 5
not ok 6



=== TEST 4: factorial.js w/o args
--- args
factorial.js
--- stderr
usage: factorial.js N
--- stdout



=== TEST 5: factorial.js with arg 3
--- args
factorial.js 3
--- stderr
--- stdout
6



=== TEST 6: factorial.js with arg 0
--- args
factorial.js 0
--- stderr
--- stdout
1



=== TEST 7: factorial.js with invalid args -2
--- args
factorial.js -2
--- stdout
--- stderr
usage: factorial.js N




=== TEST 8: factorial.js with invalid args 'a'
--- args
factorial.js a
--- stdout
--- stderr
usage: factorial.js N
