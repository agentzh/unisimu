vpath %.pl script

all: test.png test2.png class.png

force:

class.png:
	perl -S -Ilib umlclass.pl -M SM -p "Sim::|SM" -o $@

%.png: %.xml
	seq2rast -c #80ffff -a yellow -w 150 $< > $@

%.svg: %.xml
	seq2svg -c #80ffff -a yellow -w 230 $< > $@

%.xml: %.pl %.methods force
	genericseq UML::Sequence::PerlSeq $*.methods $< 5 > $@

%.methods: %._methods
	-perl -d:CallSeq $*.pl
	perl script/gen_methods.pl $< > $@

SHELL = cmd
