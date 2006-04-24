#: assign.t

use t::Kid_AST_Logic;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- ast_logic
a:=3+5*2



=== TEST 2
--- kid
a:=3+2; b:=5*6  ;c:=1*2
--- ast_logic
(and (and a:=3+2 b:=5*6) c:=1*2)
