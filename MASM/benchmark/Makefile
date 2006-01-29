RM_F = perl -MExtUtils::Command -e rm_f
WRAPPERS = prog timer

.SUFFIXES: .obj .exe .c

all: test2.exe test.exe

.obj.exe:
	link /nologo $<

.c.obj:
	cl /nologo /c $<

test.c: test.c.tt $(WRAPPERS)
	tpage test.c.tt > $@

test2.c: test2.c.tt $(WRAPPERS)
	tpage test2.c.tt > $@

clean:
	$(RM_F) *.exe *.obj *.c
