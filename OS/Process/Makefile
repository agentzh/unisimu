RM_F = perl -MExtUtils::Command -e rm_f

all: prio_disp.exe

prio_disp.exe: prio_disp.obj
	link /nologo prio_disp.obj

prio_disp.obj: prio_disp.cpp
	cl /EHsc /nologo /c prio_disp.cpp

clean:
	$(RM_F) prio_disp.exe prio_disp.obj

test: out out~ run_test

out: prio_disp.exe
	prio_disp > $@

out~: prio_disp.pl
	perl prio_disp.pl -b > $@

run_test:
	diff out out~
