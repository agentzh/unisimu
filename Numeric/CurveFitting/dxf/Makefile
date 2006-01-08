RM_F = perl -MExtUtils::Command -e rm_f

all: coord-trans.html surf-fit.html surf-fit2.html

coord-trans.html: coord-trans.pod
	podhtm -s Active.css coord-trans.pod

coord-trans.pod: coord-trans.podx
	podx coord-trans.podx

surf-fit.html: surf-fit.pod
	podhtm -s Active.css surf-fit.pod

surf-fit.pod: surf-fit.podx
	podx surf-fit.podx

surf-fit2.html: surf-fit2.pod
	podhtm -s Active.css surf-fit2.pod

surf-fit2.pod: surf-fit2.podx
	podx surf-fit2.podx

clean::
	$(RM_F) *.pod *.tmp
