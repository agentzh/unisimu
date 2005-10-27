RM_F = perl -MExtUtils::Command -e rm_f

all: dec.exe out.exe temp.exe

.SUFFIXES: .exe .obj .asm

.obj.exe:
	link $<;

.asm.obj:
	masm $<;

out.asm: dec.asm
	masmd dec.asm > $@

temp.asm: cyf.asm
	masmd cyf.asm > $@

clean:
	$(RM_F) *.exe *.obj out.asm temp.asm
