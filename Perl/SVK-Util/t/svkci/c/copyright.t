#: copyright.t
#: Test the copyright year checking feature of `svkci.pl'
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-04 2006-03-04

use t::SVK::Commit;

plan tests => 3 * blocks;

run_tests;

__DATA__

=== TEST 0:
--- filename:   baz
--- content
//: Copyright (c) 2004, 2005 Agent Zhang

blah blah blah

--- stdout_like
info: checking file baz...[.\n]*
--- stderr_like
svk status invoked\.
.+
\* baz: line 1: error: Copyright out-of-date. It's ^year^ already.

For total 1 fatal error. Commiting Stop.
--- success:      false
--- svk_status
M   baz



=== TEST 1:
--- filename:   baz
--- content
//: Copyright (c) 2004-^year^ Agent Zhang

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
