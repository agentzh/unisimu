RM_F = perl -MExtUtils::Command -e rm_f

all: comb2.html comb.pdf

comb2.html: comb2.pod
	podhtm -s Active.css comb2.pod

comb2.pod: comb2.podx
	podx comb2.podx

comb.pdf: comb.tex
	pdflatex comb.tex

clean:
	$(RM_F) comb2.html comb2.pod *.aux *.idx *.log *.pdf *.tmp
