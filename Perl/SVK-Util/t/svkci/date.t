#: date.t
#: Test the date checking feature
#: Copyright (C) 2006 Agent Zhang
#: 2006-02-04 2006-02-04

use t::SVK::Commit;
use Date::Simple qw(today);

plan tests => 3 * blocks;

run_tests;

__DATA__

=== TEST 0: create < modif > today
--- filename:   foo
--- content
#: ^today^ ^tomorrow^

blah blah blah

--- stdout_like
info: checking file foo...[.\n]*
--- stderr_like
svk status invoked\.
.+
\* foo: line 1: error: Last Modified Date \(^tomorrow^\) is more recent than today \(^today^\).

For total 1 fatal error. Commiting Stop.
--- success:    false
--- svk_status
M foo



=== TEST 1: create < modif > today
file foo not appear in the output of `svk status'
--- filename:   foo
--- content
#: ^today^ ^tomorrow^

blah blah blah

--- stdout_like
info: checking file barbar...[.\n]*
--- stderr
svk status invoked.
svk ci invoked.
--- success:    true
--- svk_status
M barbar



=== TEST 2: create = modif = today
--- filename:   bar
--- content
#: ^today^ ^today^

blah blah blah

--- stdout_like
info: checking file bar...[.\n]*
--- stderr_like
svk status invoked\.
(.|\n)*
svk ci invoked\.
--- success:    true
--- svk_status
M bar



=== TEST 3: create > modif < today
--- filename:   foo
--- content
#: ^today^ ^yesterday^

blah blah blah

--- stdout_like
info: checking file foo...[.\n]*
--- stderr_like
svk status invoked\.
(.|\n)*
\* foo: line 1: error: Create Date \(^today^\) is more recent than Last Modified Date \(^yesterday^\).
\* foo: line 1: error: Last Modified Date \(^yesterday^\) is not today \(^today^\).

For total 2 fatal errors. Commiting Stop.
--- success:    false
--- svk_status
M foo
