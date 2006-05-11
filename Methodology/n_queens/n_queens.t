#: n_queens.t
#: 2006-05-11 2006-05-11

use strict;
use warnings;

use Test::More 'no_plan';
BEGIN { use_ok('n_queens'); }

# ------------------------
# Test n::Queens::can_put
# ------------------------

*can_put = \&n::Queens::can_put;

# empty chessboard
ok can_put(0);
ok can_put(1);
ok can_put(2);
ok can_put(3);

# chessboard with 1 queen already on (0,0)
ok ! can_put(0, 0);
ok ! can_put(0, 1);
ok   can_put(0, 2);
ok   can_put(0, 3);
ok   can_put(0, 4);

# chessboard with 2 queens already on
#   (0,0) and (2,1) respectively
ok ! can_put(0, 0, 2);
ok ! can_put(1, 0, 2);
ok ! can_put(2, 0, 2);
ok ! can_put(3, 0, 2);
ok   can_put(4, 0, 2);
ok   can_put(5, 0, 2);

# chessboard with 2 queens already on (0,0) and (3,1)
ok ! can_put(0, 0, 3);
ok   can_put(1, 0, 3);
ok ! can_put(2, 0, 3);
ok ! can_put(3, 0, 3);
ok ! can_put(4, 0, 3);
ok   can_put(5, 0, 3);
ok   can_put(6, 0, 3);

# -------------------------
# Test n::Queens::gen_sols
# -------------------------

*gen_sols = \&n::Queens::gen_sols;

my @sols = gen_sols(0);
is_deeply \@sols, [], 'The 0-queen problem';

@sols = gen_sols(1);
is_deeply \@sols, [[0]], 'The 1-queen problem';

@sols = gen_sols(2);
is_deeply \@sols, [], 'The 2-queens problem';

@sols = gen_sols(3);
is_deeply \@sols, [], 'The 3-queens problem';

@sols = gen_sols(4);
is_deeply \@sols, [[1,3,0,2],[2,0,3,1]], 'The 4-queens problem';

@sols = gen_sols(8);
is scalar(@sols), 92, 'The 8-queens problem has 92 solutions';
