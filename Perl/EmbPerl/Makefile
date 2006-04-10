RM_F = perl -MExtUtils::Command -e rm_f

CFLAGS_C   = /c /nologo  -nologo -Gf -W3 -MD -Zi -DNDEBUG -O1 -DWIN32 -D_CONSOLE -DNO_STRICT -DHAVE_DES_FCRYPT  -DNO_HASH_SEED -DPERL_IMPLICIT_CONTEXT -DPERL_IMPLICIT_SYS -DUSE_PERLIO -DPERL_MSVCRT_READFIX  -I"D:\Perl\lib\CORE"  

CFLAGS_CPP = /nologo /c /Za /EHsc

LIB_FLAGS = /nologo /out:embperl.lib /NODEFAULTLIB

PERL_LIB_FILE = perl58.lib

all : embperl.lib

embperl.lib : perlxsi.obj plstub.obj plinterp.obj plsv.obj plav.obj plhv.obj
	lib $(LIB_FLAGS) perlxsi.obj plstub.obj \
					plinterp.obj plsv.obj plav.obj plhv.obj \
					$(PERL_LIB_FILE)

%.obj : %.c
	cl $(CFLAGS_C) $<

pl%.obj : pl%.cpp plproxy.hpp embperl.h
	cl $(CFLAGS_CPP) $<

clean:
	$(RM_F) *.obj *.lib t/*.obj t/*.exe vc70.pdb t/*.t

test: all t\interp.t.exe t\sv.t.exe t\av.t.exe t\hv.t.exe
	cd t && gen_t.bat && cd ..
	prove t/*.t

%.t.exe: %.t.obj embperl.lib
	link /out:$@ /nologo $(notdir $^) /NODEFAULTLIB:MSVCRT

%.t.obj: %.t.cpp embperl.h
	cl /EHsc /I. /c /nologo $<

install: all
	cp embperl.lib WinRel/
	cp embperl.h WinRel/

.PHONY: all test clean install
