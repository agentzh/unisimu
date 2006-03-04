#: palindrome.t
#: test palindrome.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-27 2006-03-04

use Test::Cmd::Base;

plan tests => 1 * blocks;

my $script = shift || 'palindrome.pl';

if ($script =~ /\.pl$/i) {
    run_cmd_tests("$^X $script");
} else {
    run_cmd_tests($script);
}

__DATA__

=== TEST 1
--- stdin
abba
--- stdout
palindrome



=== TEST 2
--- stdin
abbaa
--- stdout
not palindrome



=== TEST 3
--- stdin
a bb a
--- stdout
palindrome



=== TEST 4
--- stdin
a ba
--- stdout
combinational palindrome
