RM_F = perl -MExtUtils::Command -e rm_f
PNG_FILES = line.png line2.png circles.png dda.png bresenham.png

.PHONY: all test clean release

.PRECIOUS: %.exe

all: gdpp.lib

gdpp.lib: gdpp.obj
	lib /nologo /out:$@ $< embperl.lib

gdpp.obj: gdpp.cpp gdpp.h
	cl /EHsc /c /nologo $<

test: all $(PNG_FILES) t\imcmp.exe
	-t/imcmp.exe t/~line.png t/~line2.png > imcmp.out
	cmp line.png t\~line.png
	cmp line2.png t\~line2.png
	cmp circles.png t\~circles.png
	diff imcmp.out t\~imcmp.out
	cmp dda.png t\~line.png
	cmp bresenham.png t\~line.png

%.png: t\%.exe
	$<

%.exe: %.cpp gdpp.lib
	cl /I. /EHsc /nologo /o $@ $^ /link /NODEFAULTLIB:MSVCRT

clean:
	$(RM_F) *.lib *.obj t/*.exe t/*.obj *.png imcmp.out

release: all
	cp gdpp.h release/
	cp gdpp.lib release/
	cp t/*.cpp release/
	tar.gz release
	cp release.tar.gz GD++.tar.gz
