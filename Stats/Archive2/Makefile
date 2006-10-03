NAME = Archive2
HS_FILES = $(wildcard *.hs)
PL_FILES = $(wildcard *.pl)
HSYML_FILES = $(patsubst %,%.yml, $(HS_FILES))
PLYML_FILES = $(patsubst %,%.yml, $(PL_FILES))
RM_F = perl -MExtUtils::Command -e rm_f

all: $(NAME).html

$(NAME).html: $(PLYML_FILES) $(HSYML_FILES) ../podgen.pl
	perl ../podgen.pl -o $(NAME).pod -t ../pod.tt -n $(NAME) $(PL_FILES) $(HS_FILES)
	podhtm -s ../Active.css $(NAME).pod

%.pl.yml: %.pl ../ymlgen.pl
	perl ../ymlgen.pl $<

%.hs.yml: %.hs %.hs.exe ../ymlgen.pl
	perl ../ymlgen.pl $<

%.hs.exe: %.hs
	ghc -o $@ $<

clean:
	$(RM_F) *.o *.exe *.hi *.yml *.pod *.tmp
