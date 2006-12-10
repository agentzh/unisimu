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

vpath %.grammar grammar
vpath %.pl script

all: lib/XClips/Compiler/Base.pm

lib/XClips/Compiler/Base.pm: xclips.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< XClips::Compiler::Base
	$(mv_f) Base.pm $@

%.clp: %.xclp xclips.pl lib/XClips/Compiler.pm lib/XClips/Compiler/Base.pm vrg-sugar.xclp
	$(xclp) $<

test: all
	prove -Ilib t/*.t

clean: clean
	$(rm_f) lib/XClips/Compiler/Base.pm
	clips-cover -d

install:
	pl2bat script/xclips.pl script/clips-cover.pl
	$(cp_f) script/xclips.bat $(PERL_BIN)
	$(cp_f) script/clips-cover.bat $(PERL_BIN)
	$(cp_f) lib/Clips/Batch.pm $(PERL_LIB)/Clips
	$(cp_f) lib/Clips/GraphViz.pm $(PERL_LIB)/Clips
	$(cp_f) lib/XClips/Compiler.pm $(PERL_LIB)/XClips
	$(cp_f) lib/XClips/Compiler/Base.pm $(PERL_LIB)/XClips/Compiler
