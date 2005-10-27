RM_F = perl -MExtUtils::Command -e rm_f

all: abs-cmp.exe out.exe

.SUFFIXES: .obj .exe .asm

.obj.exe:
	link $<;

.asm.obj:
	masm -l $<;

out.asm: abs-cmp.asm
	perl ../masmd.pl abs-cmp.asm > $@

clean:
	$(RM_F) out.asm *.exe *.obj *.lst
