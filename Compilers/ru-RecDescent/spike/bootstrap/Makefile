CP_F = perl -MFile::Copy -e "File::Copy::cp(@ARGV)"
RM_F = perl -MExtUtils::Command -e rm_f
SPIKE = ../spike.pl
FILES = spike_parser2.pm spike_emitter.pm test_spike.pm

.PHONY: all test clean

all: $(FILES)

spike_parser2.pm: spike_parser2.grammar $(SPIKE)
	perl $(SPIKE) -m -n Spike::Parser $<

spike_emitter.pm: ../spike_emitter.pm
	$(CP_F) $< $@

test_spike.pm: ../test_spike.pm
	$(CP_F) $< $@

test: all
	prove -I. ../*.t

clean:
	$(RM_F) $(FILES)
