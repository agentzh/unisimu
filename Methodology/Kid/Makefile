MV_F = perl -MFile::Copy -e "File::Copy::mv(@ARGV)"
RM_F = perl -MExtUtils::Command -e rm_f
PM_FILES = lib/Kid/Expression.pm lib/Kid/Term.pm lib/Kid/Parser.pm \
           lib/Kid/Statements.pm
T_MODULES = t/Kid_Maple.pm t/Kid_XML.pm t/Kid_Perl.pm \
            t/Kid_AST_Logic.pm t/Kid_AST_Logic_Disjoint.pm

.PHONY: all test clean

all: $(PM_FILES)

lib/Kid/Expression.pm: lib/leftop.pm.tt
	tpage --define parent=expression --define child=term $< > $@

lib/Kid/Term.pm: lib/leftop.pm.tt
	tpage --define parent=term --define child=factor $< > $@

lib/Kid/Statements.pm: lib/leftop.pm.tt
	tpage --define parent=statement_list --define child=statement \
		--define op=no --define key=statement(s) $< > $@

lib/Kid/Parser.pm: lib/kid.grammar
	perl -MParse::RecDescent - $< Kid::Parser
	$(MV_F) Parser.pm lib/Kid/

test: all $(T_MODULES)
	prove -Ilib t/*/*.t t/*.t

$(T_MODULES): t/Kid_XXX.pm.tt
	tpage --define name=$(patsubst t/Kid_%.pm,%,$@) $< > $@

clean:
	$(RM_F) $(PM_FILES) t/0*test.pl t/0*test.xml t/0*test.mpl $(T_MODULES)
