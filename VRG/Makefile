SHELL := cmd

xpro := perl xpro.pl
xclp := perl -Ilib script/xclips.pl -I knowledge

rm_f = perl -MExtUtils::Command -e rm_f
mv_f = perl -MExtUtils::Command -e mv

xpro_files := $(wildcard *.xpro)
pro_files  := $(patsubst %.xpro,%.pro, $(xpro_files))

clp_files  := $(patsubst %,knowledge/%,vectorize.clp vector-eval.clp anti-vectorize.clp)

vpath %.xclp knowledge
vpath %.grammar grammar

all: clips_all

clips_all: lib/VRG/Compiler.pm lib/CLIPS/Batch.pm $(clp_files)

prolog_all: $(pro_files)

lib/XClips/Compiler/Base.pm: xclips.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< XClips::Compiler::Base
	$(mv_f) Base.pm $@

lib/VRG/Compiler.pm: vrgs.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< VRG::Compiler
	$(mv_f) Compiler.pm $@

%.pro: %.xpro xpro.pl
	$(xpro) $<

knowledge/vectorize.clp: preprocess.xclp

%.clp: %.xclp xclips.pl lib/XClips/Compiler.pm lib/XClips/Compiler/Base.pm vrg-sugar.xclp
	$(xclp) $<

testall: clips_all prolog_all
	prove -Ilib t/*.t

test: clips_all
	prove -Ilib t/sanity2.t

clean:
	$(rm_f) *.pro 0*.xpro 0*.xclp *.clp *.vrg \
		lib/XClips/Compiler/Base.pm lib/VRG/Compiler.pm \
		knowledge/*.clp
