use('Test.More');

plan( {tests: 5} );

is("a", "a");
is(5, 5);
like("a", '[a-z]');
ok("abc" == "abc");
ok("abc" != "abcd");
