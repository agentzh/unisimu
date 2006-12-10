use Test::Base;
use File::Slurp;
#use Test::Differences;
use IPC::Run3 'run3';

plan tests => 3 * blocks();

my $i = 0;

my $tmpdir = 't';
mkdir $tmpdir if !-d $tmpdir;

run {
    my $block = shift;
    my ($fbase, $xclp_file);
    if ($block->xclips) {
        $fbase = "test-" . ++$i;
        $xclp_file = "$tmpdir/$fbase.xclp";
        write_file($xclp_file, $block->xclips);
    } elsif ($block->base) {
        $fbase = $block->base;
        $xclp_file = "$tmpdir/$fbase.xclp";
    } else {
        die "xclips or base sections are required - ", $block->name;
    }
    my $clp_file  = "$tmpdir/$fbase.clp";
    unlink $clp_file if -f $clp_file;
    run3 [$^X, '-Ilib', 'script/xclips.pl', '-I', 't', '-c', $xclp_file], \undef, undef, undef;
    is $?, 0, 'xclips.pl terminated successfully';
    ok -f $clp_file, "$clp_file exists - " . $block->name;
    my $clp_out = read_file($clp_file);
    #$clp_out =~ s/\W//g;
    my $expected = $block->clips;
    #$expected =~ s/\W//g;
    is $clp_out, $expected, 'clips output okay - ' . $block->name;
};

__DATA__

=== TEST 1:
--- base: preprocess
--- clips

;  preprocessing rules for vectorization 

;  syntax sugar definitions for the VRG knowledge base 

;  (line 8)
(defrule Vectorize::preprocess-8
    (and (plane ?alpha) (and (plane ?beta) (meet ?alpha ?beta ?l)))
    =>
    (assert (space-relation not_parallel ?alpha ?beta))
    (assert (space-relation on ?l ?alpha))
    (assert (space-relation on ?l ?beta))
)

;  (line 15)
(defrule Vectorize::preprocess-15
    (and (line ?l) (and (line ?m) (meet ?l ?m ?)))
    =>
    (bind ?alpha (gensym))
    (assert (plane ?alpha))
    (assert (temp ?alpha))
    (assert (space-relation on ?l ?alpha))
    (assert (space-relation on ?m ?alpha))
    (assert (space-relation not_parallel ?l ?m))
)

;  (line 16)
(defrule Vectorize::preprocess-16
    (and (line ?l) (and (plane ?alpha) (meet ?l ?alpha ?)))
    =>
    (assert (space-relation not_parallel ?l ?alpha))
    (assert (space-relation not_on ?l ?alpha))
)

;  (line 23)
(defrule Vectorize::preprocess-23
    (and (line ?l) (and (plane ?alpha) (and (line ?m) (project ?l ?alpha ?m))))
    =>
    (bind ?theta (gensym))
    (assert (plane ?theta))
    (assert (temp ?theta))
    (assert (space-relation cross ?l ?alpha))
    (assert (space-relation on ?l ?theta))
    (assert (meet ?theta ?alpha ?m))
    (assert (space-relation orthogonal ?theta ?alpha))
)

;  (line 25)
(defrule Vectorize::preprocess-25
    (and (plane ?alpha) (and (line ?l) (space-relation ?R ?alpha ?l)))
    =>
    (assert (space-relation ?R ?l ?alpha))
)



=== TEST 2:
--- base: vectorize
--- clips

(defmodule MAIN (export deftemplate initial-fact))

(defmodule Vectorize (export deftemplate ?ALL))

;  preprocessing rules for vectorization 

;  syntax sugar definitions for the VRG knowledge base 

; t\preprocess.xclp (line 8)
(defrule Vectorize::preprocess-8
    (and (plane ?alpha) (and (plane ?beta) (meet ?alpha ?beta ?l)))
    =>
    (assert (space-relation not_parallel ?alpha ?beta))
    (assert (space-relation on ?l ?alpha))
    (assert (space-relation on ?l ?beta))
)

; t\preprocess.xclp (line 15)
(defrule Vectorize::preprocess-15
    (and (line ?l) (and (line ?m) (meet ?l ?m ?)))
    =>
    (bind ?alpha (gensym))
    (assert (plane ?alpha))
    (assert (temp ?alpha))
    (assert (space-relation on ?l ?alpha))
    (assert (space-relation on ?m ?alpha))
    (assert (space-relation not_parallel ?l ?m))
)

; t\preprocess.xclp (line 16)
(defrule Vectorize::preprocess-16
    (and (line ?l) (and (plane ?alpha) (meet ?l ?alpha ?)))
    =>
    (assert (space-relation not_parallel ?l ?alpha))
    (assert (space-relation not_on ?l ?alpha))
)

; t\preprocess.xclp (line 23)
(defrule Vectorize::preprocess-23
    (and (line ?l) (and (plane ?alpha) (and (line ?m) (project ?l ?alpha ?m))))
    =>
    (bind ?theta (gensym))
    (assert (plane ?theta))
    (assert (temp ?theta))
    (assert (space-relation cross ?l ?alpha))
    (assert (space-relation on ?l ?theta))
    (assert (meet ?theta ?alpha ?m))
    (assert (space-relation orthogonal ?theta ?alpha))
)

; t\preprocess.xclp (line 25)
(defrule Vectorize::preprocess-25
    (and (plane ?alpha) (and (line ?l) (space-relation ?R ?alpha ?l)))
    =>
    (assert (space-relation ?R ?l ?alpha))
)

;  vectorization rules 

;  line-line relationships 

;  (line 19)
(defrule Vectorize::vectorize-19
    (and (line ?l) (and (line ?m) (space-relation ?R ?l ?m)))
    =>
    (assert (vector-relation ?R ?l ?m))
)

;  line-plane relationships 

;  (line 23)
(defrule Vectorize::vectorize-23
    (and (line ?l) (and (plane ?alpha) (space-relation orthogonal ?l ?alpha)))
    =>
    (assert (vector-relation parallel ?l ?alpha))
)

;  (line 24)
(defrule Vectorize::vectorize-24
    (and (line ?l) (and (plane ?alpha) (space-relation parallel ?l ?alpha)))
    =>
    (assert (vector-relation orthogonal ?l ?alpha))
)

;  (line 25)
(defrule Vectorize::vectorize-25
    (and (line ?l) (and (plane ?alpha) (space-relation cross ?l ?alpha)))
    =>
    (assert (vector-relation cross ?l ?alpha))
)

;  (line 26)
(defrule Vectorize::vectorize-26
    (and (line ?l) (and (plane ?alpha) (space-relation on ?l ?alpha)))
    =>
    (assert (vector-relation orthogonal ?l ?alpha))
)

;  (line 28)
(defrule Vectorize::vectorize-28
    (and (line ?l) (and (plane ?alpha) (space-relation not_orthogonal ?l ?alpha)))
    =>
    (assert (vector-relation not_parallel ?l ?alpha))
)

;  (line 29)
(defrule Vectorize::vectorize-29
    (and (line ?l) (and (plane ?alpha) (space-relation not_parallel ?l ?alpha)))
    =>
    (assert (vector-relation not_orthogonal ?l ?alpha))
)

;  (line 30)
(defrule Vectorize::vectorize-30
    (and (line ?l) (and (plane ?alpha) (space-relation not_cross ?l ?alpha)))
    =>
    (assert (vector-relation not_cross ?l ?alpha))
)

;  plane-plane relationships 

;  (line 34)
(defrule Vectorize::vectorize-34
    (and (plane ?alpha) (and (plane ?beta) (space-relation ?R ?alpha ?beta)))
    =>
    (assert (vector-relation ?R ?alpha ?beta))
)

;  control rules 

;  (line 37)
(defrule Vectorize::vectorize-37
    (contradiction $?)
    =>
    (halt)
)



=== TEST 3:
--- xclips
ion(Ba, 2), ion(SO4, -2) => deposit(BaSO4).
ion(Ag, 1), ion(Cl, -1) => deposit(AgCl).

deposit(?X) => printout(t, ?X, crlf).

ion(Ba, +2).
ion(Cl, -1).
ion(Cu, +2).
--- clips
;  (line 1)
(defrule test-1-1
    (and (ion Ba 2) (ion SO4 -2))
    =>
    (assert (deposit BaSO4))
)

;  (line 2)
(defrule test-1-2
    (and (ion Ag 1) (ion Cl -1))
    =>
    (assert (deposit AgCl))
)

;  (line 4)
(defrule test-1-4
    (deposit ?X)
    =>
    (printout t ?X crlf)
)
(deffacts test-1
    ;  (line 6)
    (ion Ba +2)

    ;  (line 7)
    (ion Cl -1)

    ;  (line 8)
    (ion Cu +2)
)



=== TEST 4:
--- xclips

circumfix:<[ ]>  ion
circumfix:<{ }>  deposit

[Ba, +2].
[Ba, 2], [SO4, -2] => {BaSO4}.
[Ag, 1], [Cl, -1] => {AgCl}.

--- clips
;  (line 5)
(defrule test-2-5
    (and (ion Ba 2) (ion SO4 -2))
    =>
    (assert (deposit BaSO4))
)

;  (line 6)
(defrule test-2-6
    (and (ion Ag 1) (ion Cl -1))
    =>
    (assert (deposit AgCl))
)
(deffacts test-2
    ;  (line 4)
    (ion Ba +2)
)
