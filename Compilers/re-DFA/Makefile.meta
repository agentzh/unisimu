MV_F = perl -MFile::Copy -e "File::Copy::mv(@ARGV)"
RM_F = perl -MExtUtils::Command -e rm_f

GRAMMAR = grammar/re.grammar
PM_FILES = lib/re/AST/Alternation.pm lib/re/AST/Concat.pm \
           lib/re/Parser.pm

LEFTOP_TT = template/leftop.pm.tt

.PHONY: all test clean build doc

all: $(PM_FILES) #$(SCRIPTS) $(T_MODULES) $(T_SCRIPTS)

lib/re/AST/Alternation.pm: $(LEFTOP_TT)
	tpage --define parent=alternation --define child=concat \
		--define op=no --define "key=concat(s)" $< > $@

lib/re/AST/Concat.pm: $(LEFTOP_TT)
	tpage --define parent=concat --define child=modified_atom \
		--define op=no --define "key=modified_atom(s?)" $< > $@

lib/re/Parser.pm: $(GRAMMAR)
	perl -MParse::RecDescent - $< re::Parser
	$(MV_F) Parser.pm lib/re/

test: all
	prove -Ilib t/*/*.t t/*.t

clean:
	$(RM_F) $(PM_FILES) t/re-Graph/g28.png t/re-NFA/nfa*.png t/re-DFA/dfa*.png \
	        t/re-DFA-Min/dfa*.png

#win32-build.bat: template/win32-build.tt
#	tpage $< > $@

#doc: doc/Language.html doc/Utilities.html doc/cn-zh/Journals.html

#doc/cn-zh/%.html: doc/cn-zh/%.pod
#	podhtm -s ../Active.css -o $@ $<
#	$(RM_F) *.tmp

#%.html: %.pod
#	podhtm -s Active.css -o $@ $<
#	$(RM_F) *.tmp
