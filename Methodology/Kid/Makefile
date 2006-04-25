MV_F = perl -MFile::Copy -e "File::Copy::mv(@ARGV)"

RM_F = perl -MExtUtils::Command -e rm_f

PM_FILES = lib/Kid/AST/Statements.pm lib/Kid/AST/Expression.pm lib/Kid/AST/Term.pm \
           lib/Kid/Parser.pm \

T_MODULES = t/Kid_Maple.pm t/Kid_XML.pm t/Kid_Perl.pm \
            t/Kid_Logic.pm t/Kid_Logic_Disjoint.pm \
			t/Kid_MathModel.pm

.PHONY: all test clean

all: $(PM_FILES)

lib/Kid/AST/Statements.pm: template/leftop.pm.tt
	tpage --define parent=statement_list --define child=statement \
		--define op=no --define key=statement(s) $< > $@

lib/Kid/AST/Expression.pm: template/leftop.pm.tt
	tpage --define parent=expression --define child=term $< > $@

lib/Kid/AST/Term.pm: template/leftop.pm.tt
	tpage --define parent=term --define child=factor $< > $@

lib/Kid/Parser.pm: grammar/kid.grammar
	perl -MParse::RecDescent - $< Kid::Parser
	$(MV_F) Parser.pm lib/Kid/

test: all $(T_MODULES)
	prove -Ilib t/*/*.t t/*.t

$(T_MODULES): template/t_Kid.pm.tt
	tpage --define name=$(patsubst t/Kid_%.pm,%,$@) $< > $@

clean:
	$(RM_F) $(PM_FILES) t/0*test.pl t/0*test.xml t/0*test.mpl \
	        t/0*test.mm $(T_MODULES)
