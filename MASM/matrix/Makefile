RM_F = perl -MExtUtils::Command -e rm_f

all : matrix.exe

matrix.exe : matrix.obj
	link matrix.obj;

matrix.obj : matrix.asm
	masm matrix.asm;

test : all out out~
	cmp out out~

out : matrix.exe
	matrix.exe > $@

out~ : matrix.pl
	perl matrix.pl > $@

clean :
	$(RM_F) matrix.exe matrix.obj out out~
