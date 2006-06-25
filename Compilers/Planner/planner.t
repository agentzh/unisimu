# planner.t
# Test planner.pl

use strict;
use warnings;

use File::Temp qw/ tempfile /;
use Test::Base;
use IPC::Run3;

plan tests => 2 * blocks() + 2 * 8;

my $plfile;
my @plfiles;

mkdir 'tmp' if !-d 'tmp';

run {
    my $block = shift;
    my $pln = $block->pln;
    my $name = $block->name;

    if (defined $pln) {
        my ($fh, $plnfile) =
            tempfile('PLN_XXXXXX', SUFFIX => '.pln', UNLINK => 1, DIR => 'tmp');
        #warn "Grammar File: $gmfile";
        print $fh $pln;
        close $fh;
        is system($^X, 'planner.pl', $plnfile), 0, "$name - spawn planner.pl ok";
        ($plfile = $plnfile) =~ s/\.pln$/.pl/;
        ok -f $plfile, "$name - $plfile ok";
    }

    my @cmd = ($^X, $plfile);
    my $args = $block->args;
    if ($args) {
        $args =~ s/\n//g;
        push @cmd, split(/\s+/, $args);
    }
    my ($stdin, $stdout, $stderr);
    $stdin = $block->stdin;
    run3([@cmd], \$stdin, \$stdout, \$stderr);
    my $res;
    $res += is $stdout, $block->stdout, "$name - stdout ok";
    $res += is $stderr, $block->stderr, "$name - stderr ok";
    #warn "!@!@ res == $res\n";
    push @plfiles, $plfile if $res == 2;
};

for my $plfile (@plfiles) {
    unlink $plfile;
}

__DATA__

=== TEST 1: Counter (-v 0)
--- pln

{ process(0); }

var a;

a: { $a < 5 } { $a++; }

a*;

{ print "a = $a\n"; }

--- stdout
a = 5
--- stderr



=== TEST 2: Counter (-v 1)
--- args: -v 1
--- stdout
a = 5
--- stderr
a
a a
a a a
a a a a
a a a a a

<<MATCH>>



=== TEST 3: Counter (-v 2)
--- args: -v 2
--- stdout
a = 5
--- stderr
  trying repet_0...
a
a a
a a a
a a a a
a a a a a

<<MATCH>>



=== TEST 4: Counter (-v 3)
--- args: -v 4
--- stdout
a = 5
--- stderr
  trying repet_0...
  trying atom_a...
a
  trying atom_a...
a a
  trying atom_a...
a a a
  trying atom_a...
a a a a
  trying atom_a...
a a a a a
  trying atom_a...

<<MATCH>>



=== TEST 5: Backtracking Counter
Test the backtracking behavior of *
--- pln

{ process(0); }

var a;

a: { $a < 5 } { $a++ }

a*a{5};

{ print "a = $a\n"; }

--- args
-v 1
--- stdout
a = 5
--- stderr
a
a a
a a a
a a a a
a a a a a
a a a a a
a a a a
a a a a a
a a a
a a a a
a a a a a
a a
a a a
a a a a
a a a a a
a
a a
a a a
a a a a
a a a a a

<<MATCH>>



=== TEST 6: Regex Matcher
--- pln
<wrapper>

var str, pos;

a: { substr($str, $pos, 1) eq 'a' } { $pos++ }
b: { substr($str, $pos, 1) eq 'b' } { $pos++ }

(a|b)*(a a|b b)(a|b)*;

--- args
aaa 0
--- stdout
a a a
--- stderr



=== TEST 7: Regex Matcher (bbaaab)
--- args
-v 1 bbaaab 0
--- stdout
b b a a a b
--- stderr
b
b b
b b a
b b a a
b b a a a
b b a a a b
b b a a a b
b b a a a
b b a a
b b a a a
b b a a a b

<<MATCH>>



=== TEST 8: Regex Matcher (baba)
--- args
baba 0
--- stdout
--- stderr
error: no solution found.



=== TEST 9: Regex Matcher (-v 1 baba)
--- args
-v 1 baba 0
--- stdout
--- stderr
b
b a
b a b
b a b a
b a b a
b a b
b a
b

<<FAIL>>
error: no solution found.



=== TEST 10: Linear State Machine
--- pln

<wrapper>

var state;

start :
    { $state eq 'a' }
    { say ("starting now..."); $state = 'b'; }

    next :  
        { $state eq 'b' }
        { say ("next, i should..."); $state = 'c'; }

    then :
        { $state eq 'c' }
        { say ("and then..."); $state = 'd'; }

end :
    { $state eq 'd' }
    { say ("finally..."); $state = 'a'; }

(start | next | then | end){4};

{ sub say { print @_, "\n" } }

--- args: a
--- stdout
starting now...
next, i should...
and then...
finally...
start next then end
--- stderr



=== TEST 11: Linear State Machine
--- args: b
--- stdout
next, i should...
and then...
finally...
starting now...
next then end start
--- stderr



=== TEST 12: Linear State Machine
--- pln

<wrapper>

var state;

start: { $state == 0 } { $state = 1; }
next : { $state == 1 } { $state = 2; }
then : { $state == 2 } { $state = 3; }
end  : { $state == 3 } { $state = 0; }

(end | then | next | start){6};

--- args
0
--- stdout
start next then end start next
--- stderr



=== TEST 13: Linear State Machine
--- pln

<wrapper>

var state;

start: { $state == 0 } { $state = 1; }
next : { $state == 1 } { $state = 2; }
then : { $state == 2 } { $state = 3; }
end  : { $state == 3 } { $state = 0; }

(end | (then | next) | start){3};

--- args
0
--- stdout
start next then
--- stderr



=== TEST 14: Linear State Machine
--- pln

<wrapper>

var state;

start: { $state == 0 } { $state = 1; }
next : { $state == 1 } { $state = 2; }
then : { $state == 2 } { $state = 3; }
end  : { $state == 3 } { $state = 0; }

(end | (then | next)+ | start){3};

--- args
0
--- stdout
start next then end
--- stderr



=== TEST 15: Linear State Machine
--- args
4
--- stdout
--- stderr
error: no solution found.



=== TEST 16: Linear State Machine
--- pln

<wrapper>

var state;

start: { $state == 0 } { $state = 1; }
next : { $state == 1 } { $state = 2; }
then : { $state == 2 } { $state = 3; }
end  : { $state == 3 } { $state = 0; }

(end | (then | next)* | start){3};

--- args
-v 2 0
--- stdout
--- stderr
  trying repet_1...
  trying repet_0...
  trying repet_0...
  trying repet_0...

<<MATCH>>
warning: no action performed.



=== TEST 17: Linear State Machine
--- args
-v 3 0
--- stdout
--- stderr
  trying repet_1...
  trying altern_1...
  trying repet_0...
  trying altern_0...
  trying altern_1...
  trying repet_0...
  trying altern_0...
  trying altern_1...
  trying repet_0...
  trying altern_0...

<<MATCH>>
warning: no action performed.



=== TEST 18: Linear State Machine
--- args
-v 4 0
--- stdout
--- stderr
  trying repet_1...
  trying altern_1...
  trying atom_end...
  trying repet_0...
  trying altern_0...
  trying atom_then...
  trying atom_next...
  trying altern_1...
  trying atom_end...
  trying repet_0...
  trying altern_0...
  trying atom_then...
  trying atom_next...
  trying altern_1...
  trying atom_end...
  trying repet_0...
  trying altern_0...
  trying atom_then...
  trying atom_next...

<<MATCH>>
warning: no action performed.



=== TEST 19: Linear State Machine
--- args
-v 1 3
--- stdout
end
--- stderr
end

<<MATCH>>
