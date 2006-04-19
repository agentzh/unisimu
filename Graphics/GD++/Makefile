RM_F = perl -MExtUtils::Command -e rm_f

.PHONY: all test clean release

all: gdpp.lib

gdpp.lib: gdpp.obj
	lib /nologo /out:$@ $< embperl.lib

gdpp.obj: gdpp.cpp gdpp.h
	cl /EHsc /c /nologo $<

test: all t\line.exe t\line2.exe t\circles.exe t\imcmp.exe t\dda.exe
	t\line.exe
	t\line2.exe
	t\circles.exe
	-t/imcmp.exe t/~line.png t/~line2.png > imcmp.out
	t\dda.exe
	cmp line.png t\~line.png
	cmp line2.png t\~line2.png
	cmp circles.png t\~circles.png
	diff imcmp.out t\~imcmp.out
	cmp dda.png t\~line.png

%.exe: %.cpp gdpp.lib
	cl /I. /EHsc /nologo /o $@ $^ /link /NODEFAULTLIB:MSVCRT

clean:
	$(RM_F) *.lib *.obj t/*.exe t/*.obj *.png imcmp.out

release:
	cp gdpp.h release/
	cp gdpp.lib release/
	cp t/*.cpp release/
	tar.gz release
	cp release.tar.gz GD++.tar.gz
