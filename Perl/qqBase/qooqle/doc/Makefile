RM_F = perl -MExtUtils::Command -e rm_f
CSS_FILE = image/Active.css
NAME = qooqle

all: qooqle.html dbques.html

$(NAME).html: $(NAME).pod
	podhtm -s $(CSS_FILE) $<

$(NAME).pod: $(NAME).podx concept.cm image/concept.png
	podx $(NAME).podx

image/concept.png: concept.cm
	er-diag.bat -o $@ -s 5x4 $<

clean::
	$(RM_F) *.tmp $(NAME).pod

dbques.html: dbques.pod
	podhtm -s $(CSS_FILE) $<
