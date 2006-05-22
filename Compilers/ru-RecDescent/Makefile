MV_F = perl -MFile::Copy -e "File::Copy::mv(@ARGV)"
RM_F = perl -MExtUtils::Command -e rm_f

GRAMMAR = grammar/ru.grammar
PM_FILES = lib/ru/AST/Components.pm lib/ru/AST/Productions.pm \
           lib/ru/AST/Production.pm lib/ru/Parser.pm

LEFTOP_TT = template/leftop.pm.tt

.PHONY: all test clean build doc

all: $(PM_FILES) #$(SCRIPTS) $(T_MODULES) $(T_SCRIPTS)

lib/ru/AST/Components.pm: $(LEFTOP_TT)
	tpage --define parent=component_list --define child=component \
		--define op=no --define "key=component(s)" $< > $@

lib/ru/AST/Productions.pm: $(LEFTOP_TT)
	tpage --define parent=production_list --define child=production \
		--define op=no --define "key=production(s)" $< > $@

lib/ru/AST/Production.pm: $(LEFTOP_TT)
	tpage --define parent=production --define child=item \
		--define op=no --define "key=item(s)" $< > $@

lib/ru/Parser.pm: $(GRAMMAR)
	perl -MParse::RecDescent - $< ru::Parser
	$(MV_F) Parser.pm lib/ru/

test: all
	prove -Ilib t/*/*.t t/*.t

clean:
	$(RM_F) $(PM_FILES) #t/re-Graph/g28.png t/re-NFA/nfa*.png t/re-DFA/dfa*.png \
	        #t/re-DFA-Min/dfa*.png

#win32-build.bat: template/win32-build.tt
#	tpage $< > $@

#doc: doc/Language.html doc/Utilities.html doc/cn-zh/Journals.html

#doc/cn-zh/%.html: doc/cn-zh/%.pod
#	podhtm -s ../Active.css -o $@ $<
#	$(RM_F) *.tmp

#%.html: %.pod
#	podhtm -s Active.css -o $@ $<
#	$(RM_F) *.tmp
