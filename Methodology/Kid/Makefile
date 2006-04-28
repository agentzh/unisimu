MV_F = perl -MFile::Copy -e "File::Copy::mv(@ARGV)"
RM_F = perl -MExtUtils::Command -e rm_f

GRAMMAR = grammar/kid.grammar
PM_FILES = lib/Kid/AST/Statements.pm lib/Kid/AST/Expressions.pm \
           lib/Kid/AST/Expression.pm lib/Kid/AST/Term.pm \
	   lib/Kid/AST/Identifiers.pm \
           lib/Kid/Parser.pm

LEFTOP_TT = template/leftop.pm.tt
T_MODULES = t/Kid_Maple.pm t/Kid_XML.pm t/Kid_Perl.pm \
            t/Kid_Logic.pm t/Kid_Logic_Disjoint.pm \
            t/Kid_MathModel.pm t/Kid_MathModel_Eval.pm \
	    t/Kid_Proc.pm t/Kid_Kid.pm

SCRIPT_TT = template/kid2xxx.pl.tt
SCRIPTS = script/kid2xml.pl script/kid2mpl.pl script/kid2pl.pl script/kid2mm.pl \
          script/kid2mms.pl

T_SCRIPT_TT = template/kid2xxx.t.tt
T_SCRIPTS = $(patsubst %.pl,t/%.t,$(SCRIPTS))

.PHONY: all test clean

all: $(PM_FILES) $(SCRIPTS)

lib/Kid/AST/Statements.pm: $(LEFTOP_TT)
	tpage --define parent=statement_list --define child=statement \
		--define op=no --define key=statement(s) $< > $@

lib/Kid/AST/Expressions.pm: $(LEFTOP_TT)
	tpage --define parent=expression_list --define child=expression \
		--define op=no --define key=expression(s) $< > $@

lib/Kid/AST/Identifiers.pm: $(LEFTOP_TT)
	tpage --define parent=identifier_list --define child=identifier \
		--define op=no --define key=identifier(s) $< > $@

lib/Kid/AST/Expression.pm: $(LEFTOP_TT)
	tpage --define parent=expression --define child=term $< > $@

lib/Kid/AST/Term.pm: $(LEFTOP_TT)
	tpage --define parent=term --define child=factor $< > $@

lib/Kid/Parser.pm: $(GRAMMAR)
	perl -MParse::RecDescent - $< Kid::Parser
	$(MV_F) Parser.pm lib/Kid/

$(SCRIPTS): $(SCRIPT_TT)
	tpage --define ext=$(patsubst script/kid2%.pl,%,$@) $< > $@

$(T_SCRIPTS): $(T_SCRIPT_TT)
	tpage --define ext=$(patsubst t/script/kid2%.t,%,$@) $< > $@

test: all $(T_MODULES) $(T_SCRIPTS)
	prove -Ilib t/*/*.t t/*.t

$(T_MODULES): template/t_Kid.pm.tt
	tpage --define name=$(patsubst t/Kid_%.pm,%,$@) $< > $@

clean:
	$(RM_F) $(PM_FILES) t/script/0*test.pl t/script/0*test.xml t/script/0*test.mpl \
	        t/script/0*test.mm* $(T_MODULES) $(SCRIPTS) $(T_SCRIPTS)
