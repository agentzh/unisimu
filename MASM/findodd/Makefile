RM_F = perl -MExtUtils::Command -e rm_f

all: findodd.exe tmp.exe

.SUFFIXES: .exe .asm .obj

.obj.exe:
	link /nologo $<;

.asm.obj:
	masm -l $<;

tmp.asm: findodd.asm
	perl ..\\masmd.pl findodd.asm > $@

clean:
	$(RM_F) *.exe *.obj tmp.asm
