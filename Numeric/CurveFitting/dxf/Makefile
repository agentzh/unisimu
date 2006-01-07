RM_F = perl -MExtUtils::Command -e rm_f

all: coord-trans.html

coord-trans.html: coord-trans.pod
	podhtm -s Active.css coord-trans.pod

coord-trans.pod: coord-trans.podx
	podx coord-trans.podx

clean::
	$(RM_F) coord-trans.pod *.tmp
