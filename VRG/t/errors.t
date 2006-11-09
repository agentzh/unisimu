use Test::Base;
use IPC::Run3;
use File::Slurp;

plan tests => 2 * blocks();

my $count = 0;
run {
    my $blocks = shift;
    my $name = $blocks->name;
    my $id = sprintf("%03d", ++$count);
    my $fname = "$id.vrg";
    #warn "fname = ", $fname, "\n";
    write_file($fname, $blocks->{vrg});
    run3([
        $^X, 'script/vrgs.pl', $fname,
    ], \undef, \undef, \my $stdout);
    ok $?, "$name - vrg-run.pl dies as expected";
    my $pat = quotemeta($blocks->error);
    like $stdout, qr/$pat/, "$name - error message ok";
};

__DATA__

=== TEST 1: define the variable a twice
--- vrg

line a;
plane b, c;
plane a;

a T b => a T b;

--- error
error: line 3: variable "a" defined twice. see the original version (type "line") at line 1.



=== TEST 2: define the variable b twice
--- vrg

line a;
plane b, c;
plane b;

a T b => a T b;

--- error
error: line 3: variable "b" defined twice. see the original version (type "plane") at line 2.



=== TEST 3: undefined variable c
--- vrg

line a, b;

a T b 
  =>
a T c;

--- error
error: line 5: variable "c" undefined.



=== TEST 4: undefined varaible used in predicates
--- vrg

line a, b;
meet(a, b, P)
  =>
a T b;

--- error
error: line 2: variable "P" undefined.



=== TEST 5: too many arguments for "meet"
--- vrg

line a, b, c;
point P;
meet(a, b, c, P) => a T b;

--- error
error: line 3: too many arguments for meet/3.



=== TEST 6: too few arguments for "meet"
--- vrg

line a, b, c;
point P;
meet(a, b) => a T b;

--- error
error: line 3: too few arguments for meet/3.



=== TEST 7: too many arguments for "project"
--- vrg

line a, b, c;
plane A;
project(a, A, b, c) => a T b;

--- error
error: line 3: too many arguments for project/3.



=== TEST 8: too few arguments for "project"
--- vrg

line a, b, c;
plane B;
project(a, B) => a T b;

--- error
error: line 3: too few arguments for project/3.
