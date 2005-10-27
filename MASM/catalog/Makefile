RM_F = perl -MExtUtils::Command -e rm_f

all: catalog.exe out.exe

.SUFFIXES: .exe .obj .asm

.obj.exe:
	link $<;

.asm.obj:
	masm -l $<;

out.asm: catalog.asm
	perl ../masmd.pl catalog.asm > $@

test: all
	catalog < in > out
	diff out out~

clean:
	$(RM_F) *.lst *.obj *.exe out output.txt out.asm
