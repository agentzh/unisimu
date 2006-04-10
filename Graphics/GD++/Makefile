RM_F = perl -MExtUtils::Command -e rm_f

.PHONY:	all	test clean

all: gdpp.lib

gdpp.lib: gdpp.obj
	lib	/nologo /out:$@	$< embperl.lib

gdpp.obj: gdpp.cpp gdpp.h
	cl /EHsc /c /nologo $<

test: all t\line.exe t\line2.exe t\circles.exe
	t\line.exe
	t\line2.exe
	t\circles.exe
	cmp	line.png t\~line.png
	cmp	line2.png t\~line2.png
	cmp	circles.png	t\~circles.png

%.exe: %.cpp gdpp.lib
	cl /I. /EHsc /nologo /o $@ $^ /link /NODEFAULTLIB:MSVCRT

clean:
	$(RM_F) *.lib *.obj t/*.exe t/*.obj *.png
