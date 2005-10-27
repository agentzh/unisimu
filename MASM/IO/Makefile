RM_F = perl -MExtUtils::Command -e rm_f

all: dec_t.exe tmp.exe hex_t.exe tmp2.exe masmd.pl

masmd.pl: masmd.pl.tt hex.asm.tt dec.asm.tt
	tpage masmd.pl.tt > $@

.SUFFIXES: .exe .asm .obj

.obj.exe:
	link /nologo $<;

.asm.obj:
	masm -l $<;

dec_t.asm: dec_t.asm.tt dec.asm.tt
	tpage dec_t.asm.tt > $@

tmp.asm: dec_t.asm
	perl ..\\masmd.pl dec_t.asm > $@

hex_t.asm: hex_t.asm.tt hex.asm.tt
	tpage hex_t.asm.tt > $@

tmp2.asm: hex_t.asm
	perl ..\\masmd.pl hex_t.asm > $@

test: out out2 run_test

run_test:
	diff out ~out
	diff out2 ~out2

out: tmp.exe
	tmp > $@

out2: tmp2.exe
	tmp2 > $@

clean:
	$(RM_F) *.exe *.obj tmp.asm hex*.asm dec*.asm tmp2.asm \
	*.lst masmd.pl out out2

install: ..\\masmd.pl

..\\masmd.pl: masmd.pl
	copy masmd.pl $@ /Y
