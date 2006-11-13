SHELL := cmd
PERL_BIN := E:/perl/bin
PERL_LIB := E:/perl/site/lib

sample_vrg_files := $(wildcard sample/*.vrg)
sample_png_files := $(patsubst %.vrg,%.png,$(sample_vrg_files))

xpro := perl xprolog/xpro.pl
xclp := perl -Ilib script/xclips.pl -I knowledge -c
vrg_run := perl -Ilib script/vrg-run.pl

rm_f = perl -MExtUtils::Command -e rm_f
mv_f = perl -MExtUtils::Command -e mv
cp_f = perl -MExtUtils::Command -e cp

xpro_files := $(wildcard xprolog/*.xpro)
pro_files  := $(patsubst %.xpro,%.pro, $(xpro_files))

clp_files  := $(patsubst %,knowledge/%,vectorize.clp vector-eval.clp \
	anti-vectorize.clp goal-match.clp)

vpath %.xclp knowledge
vpath %.grammar grammar
vpath %.pl script xprolog

all: clips_all

clips_all: lib/XClips/Compiler.pm lib/XClips/Compiler/Base.pm lib/VRG/Compiler.pm \
	lib/CLIPS/Batch.pm $(clp_files)

prolog_all: $(pro_files)

lib/XClips/Compiler/Base.pm: xclips.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< XClips::Compiler::Base
	$(mv_f) Base.pm $@

lib/VRG/Compiler.pm: vrgs.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< VRG::Compiler
	$(mv_f) Compiler.pm $@

%.pro: %.xpro xprolog/xpro.pl
	$(xpro) $<

knowledge/vectorize.clp: preprocess.xclp

%.clp: %.xclp xclips.pl lib/XClips/Compiler.pm lib/XClips/Compiler/Base.pm vrg-sugar.xclp
	$(xclp) $<

testprolog: prolog_all
	prove -Ilib xprolog/*.t

testall: prolog_all clips_all
	prove -Ilib t/*.t xprolog/*.t

test: clips_all
	prove -Ilib t/*.t

clean:
	$(rm_f) xprolog/*.pro xprolog/0*.xpro 0*.xclp *.clp *.vrg \
		sample/*.clp sample/*.xclp *.png
	perl script/clips-cover.pl -d

veryclean: clean
	$(rm_f) lib/XClips/Compiler/Base.pm lib/VRG/Compiler.pm \
		knowledge/*.clp

sample: $(sample_png_files)

sample/%.png: sample/%.vrg clips_all
	$(vrg_run) $<

install:
	pl2bat script/xclips.pl script/clips-cover.pl
	$(cp_f) script/xclips.bat $(PERL_BIN)
	$(cp_f) script/clips-cover.bat $(PERL_BIN)
	$(cp_f) lib/Clips/Batch.pm $(PERL_LIB)/Clips
	$(cp_f) lib/Clips/GraphViz.pm $(PERL_LIB)/Clips
	$(cp_f) lib/XClips/Compiler.pm $(PERL_LIB)/XClips
	$(cp_f) lib/XClips/Compiler/Base.pm $(PERL_LIB)/XClips/Compiler
