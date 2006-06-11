use('Test.More');

plan( {tests: 6} );

is("a", "a");
is(5, 5);
like("a", '[a-z]');
ok("abc" == "abc");
ok("abc" != "abcd");
is(5, 6);
