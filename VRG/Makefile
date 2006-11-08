SHELL := cmd

xpro := perl xpro.pl
xclp := perl xclips.pl

rm_f = perl -MExtUtils::Command -e rm_f

xpro_files := $(wildcard *.xpro)
pro_files  := $(patsubst %.xpro,%.pro, $(xpro_files))

xclp_files := vectorize.xclp anti-vectorize.xclp preprocess.xclp vector-eval.xclp vrg-sugar.xclp
clp_files  := vectorize.clp vector-eval.clp anti-vectorize.clp

all: clips_all

clips_all: VRG_Script_Compiler.pm CLIPSx.pm $(clp_files)

prolog_all: $(pro_files)

CLIPSx.pm: xclips.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< CLIPSx

VRG_Script_Compiler.pm: vrgs.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< VRG::Script::Compiler
	cp Compiler.pm VRG_Script_Compiler.pm

%.pro: %.xpro xpro.pl
	$(xpro) $<

vectorize.clp: preprocess.xclp

%.clp: %.xclp xclips.pl CLIPSx.pm vrg-sugar.xclp
	$(xclp) $<

testall: clips_all prolog_all
	prove t/*.t

test: clips_all
	prove t/sanity2.t

clean:
	$(rm_f) *.pro 0*.xpro 0*.xclp *.clp CLIPSx.pm *.vrg
