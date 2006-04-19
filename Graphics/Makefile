RM_F = perl -MExtUtils::Command -e rm_f
PNG_FILES = line.png line2.png circles.png dda.png bresenham.png

.PHONY: test clean

all: $(PNG_FILES)

%.png: %.pl
	perl $< > $@

test: all
	perl imcmp.pl dda.png line.png
	perl imcmp.pl bresenham.png line.png

clean:
	$(RM_F) $(PNG_FILES)
