#: char_stat.t
#: Test char_stat.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-27 2006-02-27

use Test::Cmd::Base;
use FindBin;

plan tests => 1 * blocks;

filters {
    stdin => [qw< norm >],
    stdout => [qw< norm >],
};

run_cmd_tests("$^X $FindBin::Bin/char_stat.pl");

__DATA__

=== TEST 1
--- stdin
aaa
--- stdout
a: 1.000



=== TEST 2
--- stdin
abcda
--- stdout
a: 0.400
b: 0.200
c: 0.200
d: 0.200



=== TEST 3
--- stdin
我我们爱：祖国
爱人。有
--- stdout
人: 0.100
们: 0.100
国: 0.100
我: 0.200
有: 0.100
爱: 0.200
祖: 0.100
：: 0.100
