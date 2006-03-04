#: filename.t
#: Test the file name verification feature of `svkci.pl'
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-04 2006-03-04

use t::SVK::Commit;

plan tests => 3 * blocks;

run_tests;

__DATA__

=== TEST 0:
--- filename:   baz
--- content
//
//: t/baz

blah blah blah

--- stdout_like
info: checking file baz...[.\n]*
--- stderr_like
svk status invoked\.
(.|\n)*
svk ci invoked\.
--- success:      true
--- svk_status
M   baz



=== TEST 1:
--- filename:   baz
--- content
//
//: t/baz.t

blah blah blah

--- stdout_like
info: checking file baz...[.\n]*
--- stderr_like
svk status invoked\.
(.|\n)*
\* baz: line 2: error: File name \(t/baz.t\) malformed.

For total 1 fatal error. Commiting Stop.
--- success:      false
--- svk_status
M   baz



=== TEST 2:
--- filename:   baz
--- content
#!perl
//: baz

blah blah blah

--- stdout_like
info: checking file baz...[.\n]*
--- stderr_like
svk status invoked\.
(.|\n)*
svk ci invoked\.
--- success:      true
--- svk_status
M   baz



=== TEST 3:
--- filename:   baz
--- content
#!perl
//: baz.t

blah blah blah

--- stdout_like
info: checking file baz...[.\n]*
--- stderr_like
svk status invoked\.
(.|\n)*
\* baz: line 2: error: File name \(baz.t\) malformed.

For total 1 fatal error. Commiting Stop.
--- success:      false
--- svk_status
M   baz
