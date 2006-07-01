RM_F = perl -MExtUtils::Command -e rm_f

all: report.html journals.html

%.html: %.pod
	podhtm -s images/Active.css $<
	$(RM_F) *.tmp

%.pod: %.podx
	podx $<
