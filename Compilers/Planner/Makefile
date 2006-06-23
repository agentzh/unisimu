RM_F = perl -MExtUtils::Command -e rm_f
PL_FILES = $(patsubst %.pln,%.pl,$(wildcard samples/*.pln))

all: $(PL_FILES)

samples/%.pl: samples/%.pln planner.pl planner_parser.pm planner_emitter.pm
	perl planner.pl $<

clean:
	$(RM_F) samples/*.pl
