use('Test.More');

plan( {tests: 5} );

is("a", "a");
is("a", "b");
like("a", '[a-z]');
ok("abc" == "abc");
ok("abc" != "abcd");
