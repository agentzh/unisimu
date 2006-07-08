YML_OPT = -d
PPYML_OPT = -d

all: yml ppyml html pphtml

yml:
	perl getmsn.pl $(YML_OPT) agentzh
	perl getmsn.pl $(YML_OPT) cherrychuxinyun
	perl getmsn.pl $(YML_OPT) lvbuzhang2000
	perl getmsn.pl $(YML_OPT) layesuen

ppyml:
	perl getmsn++.pl $(PPYML_OPT) agentzh
	perl getmsn++.pl $(PPYML_OPT) cherrychuxinyun
	perl getmsn++.pl $(PPYML_OPT) lvbuzhang2000
	perl getmsn++.pl $(PPYML_OPT) layesuen


html:
	perl genmsn.pl agentzh
	perl genmsn.pl cherrychuxinyun
	perl genmsn.pl lvbuzhang2000
	perl genmsn.pl layesuen

pphtml:
	perl genmsn.pl agentzh++
	perl genmsn.pl cherrychuxinyun++
	perl genmsn.pl lvbuzhang2000++
	perl genmsn.pl layesuen++
