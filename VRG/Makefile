SHELL = cmd

xpro = perl xpro.pl
xclp = perl xclp.pl

rm_f = perl -MExtUtils::Command -e rm_f

xpro_files = $(wildcard *.xpro)
pro_files  = $(patsubst %.xpro,%.pro, $(xpro_files))

xclp_files = $(wildcard *.xclp)
clp_files  = $(patsubst %.xclp,%.clp, $(xclp_files))

all: CLIPSx.pm $(pro_files) $(clp_files)

CLIPSx.pm: xclp.grammar
	perl -s -MParse::RecDescent - -RD_HINT $< CLIPSx

%.pro: %.xpro xpro.pl
	$(xpro) $<

%.clp: %.xclp xclp.pl
	$(xclp) $<

test: all
	prove *.t

clean:
	$(rm_f) *.pro 0*.xpro 0*.xclp 0*.clp vectorize.clp vector-eval.clp preprocess.clp
