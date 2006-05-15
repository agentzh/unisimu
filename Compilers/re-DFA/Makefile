MV_F = perl -MFile::Copy -e "File::Copy::mv(@ARGV)"
RM_F = perl -MExtUtils::Command -e rm_f

GRAMMAR = grammar/re.grammar
PM_FILES = lib/re/AST/Branch.pm lib/re/AST/Concat.pm \
           lib/re/Parser.pm

LEFTOP_TT = template/leftop.pm.tt
#T_MODULES = t/re_Maple.pm t/re_XML.pm t/re_Perl.pm \
#            t/re_Logic.pm t/re_Logic_Disjoint.pm \
#            t/re_MathModel.pm t/re_MathModel_Eval.pm \
#	    t/re_Proc.pm t/re_re.pm

#SCRIPT_TT = template/kid2xxx.pl.tt
#SCRIPTS = script/kid2xml.pl script/kid2mpl.pl script/kid2pl.pl script/kid2mm.pl \
#          script/kid2mms.pl script/kid2kid.pl

#T_SCRIPT_TT = template/kid2xxx.t.tt
#T_SCRIPTS = $(patsubst %.pl,t/%.t,$(SCRIPTS))

.PHONY: all test clean build doc

all: $(PM_FILES) #$(SCRIPTS) $(T_MODULES) $(T_SCRIPTS)

lib/re/AST/Branch.pm: $(LEFTOP_TT)
	tpage --define parent=branch --define child=concat \
		--define op=no --define "key=concat(s)" $< > $@

lib/re/AST/Concat.pm: $(LEFTOP_TT)
	tpage --define parent=concat --define child=modified_atom \
		--define op=no --define "key=modified_atom(s)" $< > $@

#lib/re/AST/Identifiers.pm: $(LEFTOP_TT)
#	tpage --define parent=identifier_list --define child=identifier \
#		--define op=no --define "key=identifier(s)" $< > $@

#lib/re/AST/Expression.pm: $(LEFTOP_TT)
#	tpage --define parent=expression --define child=term $< > $@

#lib/re/AST/Term.pm: $(LEFTOP_TT)
#	tpage --define parent=term --define child=factor $< > $@

lib/re/Parser.pm: $(GRAMMAR)
	perl -MParse::RecDescent - $< re::Parser
	$(MV_F) Parser.pm lib/re/

#$(SCRIPTS): $(SCRIPT_TT)
#	tpage --define ext=$(patsubst script/kid2%.pl,%,$@) $< > $@

#$(T_SCRIPTS): $(T_SCRIPT_TT)
#	tpage --define ext=$(patsubst t/script/kid2%.t,%,$@) $< > $@

test: all
	prove -Ilib t/*/*.t t/*.t

#$(T_MODULES): template/t_re.pm.tt
#	tpage --define name=$(patsubst t/re_%.pm,%,$@) $< > $@

clean:
	$(RM_F) $(PM_FILES) t/re-Graph/g28.png t/re-NFA/nfa*.png 
#t/script/0*test.pl t/script/0*test.xml t/script/0*test.mpl \
#	        t/script/0*test.mm* t/script/0*test.kid $(T_MODULES) $(SCRIPTS) \
#			$(T_SCRIPTS) Maple.log *.tmp

#win32-build.bat: template/win32-build.tt
#	tpage $< > $@

#doc: doc/Language.html doc/Utilities.html doc/cn-zh/Journals.html

#doc/cn-zh/%.html: doc/cn-zh/%.pod
#	podhtm -s ../Active.css -o $@ $<
#	$(RM_F) *.tmp

#%.html: %.pod
#	podhtm -s Active.css -o $@ $<
#	$(RM_F) *.tmp
