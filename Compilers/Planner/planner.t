# planner.t
# Test planner.pl

use strict;
use warnings;

use File::Temp qw/ tempfile /;
use Test::Base;
use IPC::Run3;

plan tests => 2 * blocks() + 1 * 10;

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
        my ($out);
        run3 [$^X, 'planner.pl', $plnfile], \undef, \undef, \$out;
        $out =~ s/^\n+//gs;
        if (defined $block->meta_err) {
            is $out, $block->meta_err, "$name - meta err";
            return;
        }
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



=== TEST 10: Regex (syntax error)
--- pln

var str, pos;

a: { substr($str, $pos, 1) eq 'a' } { $pos++ }

a{3,5}

--- meta_err

       ERROR (line 5):  no semicolon specified after the regex.

       ERROR (line 5): Invalid program: Was expecting regex but found "a{3,5}"
                       instead
Bad grammar!



=== TEST 11: Regex (test a{m,n})
--- pln
<wrapper>

var str, pos;

a: { substr($str, $pos, 1) eq 'a' } { $pos++ }

a{3,5};

--- args
-v 1 aaa 0
--- stdout
a a a
--- stderr
a
a a
a a a

<<MATCH>>



=== TEST 12: Regex (test a{m,n})
--- args
-v 1 aaaa 0
--- stdout
a a a a
--- stderr
a
a a
a a a
a a a a

<<MATCH>>



=== TEST 13: Regex (test a{m,n})
--- args
-v 1 aaaaa 0
--- stdout
a a a a a
--- stderr
a
a a
a a a
a a a a
a a a a a

<<MATCH>>



=== TEST 14: Regex (test a{m,n})
--- args
-v 1 aaaaaa 0
--- stdout
a a a a a
--- stderr
a
a a
a a a
a a a a
a a a a a

<<MATCH>>



=== TEST 15: Regex (test a{m,n})
--- args
-v 1 aa 0
--- stdout
--- stderr
a
a a

<<FAIL>>
error: no solution found.



=== TEST 16: Regex (test a{m,n})
--- args
-v 1 a 0
--- stdout
--- stderr
a

<<FAIL>>
error: no solution found.



=== TEST 17: Regex (test a{m,n})
--- pln
<wrapper>

var str, pos;

a: { substr($str, $pos, 1) eq 'a' } { $pos++ }

a{3,5}a a;

--- args
-v 1 aaaaa 0
--- stdout
a a a a a
--- stderr
a
a a
a a a
a a a a
a a a a a
a a a a a
a a a a
a a a a a

<<MATCH>>



=== TEST 18: Regex (test a{m,})
--- pln

<wrapper>

var str, pos;

a: { substr($str, $pos, 1) eq 'a' } { $pos++ }

a{2,};

--- args
-v 1 aa 0
--- stdout
a a
--- stderr
a
a a

<<MATCH>>



=== TEST 19: Regex (test a{m,})
--- args
-v 1 aaa 0
--- stdout
a a a
--- stderr
a
a a
a a a

<<MATCH>>



=== TEST 20: Regex (test a{m,})
--- args
-v 1 aaaaaa 0
--- stdout
a a a a a a
--- stderr
a
a a
a a a
a a a a
a a a a a
a a a a a a

<<MATCH>>



=== TEST 21: Regex (test a{m,})
--- args
-v 1 a 0
--- stdout
--- stderr
a

<<FAIL>>
error: no solution found.



=== TEST 22: Linear State Machine
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



=== TEST 23: Linear State Machine
--- args: b
--- stdout
next, i should...
and then...
finally...
starting now...
next then end start
--- stderr



=== TEST 24: Linear State Machine
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



=== TEST 25: Linear State Machine
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



=== TEST 26: Linear State Machine
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



=== TEST 27: Linear State Machine
--- args
4
--- stdout
--- stderr
error: no solution found.



=== TEST 28: Linear State Machine
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



=== TEST 29: Linear State Machine
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



=== TEST 30: Linear State Machine
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



=== TEST 31: Linear State Machine
--- args
-v 1 3
--- stdout
end
--- stderr
end

<<MATCH>>
