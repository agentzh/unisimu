all: test.exe

.SUFFIXES: .exe

.obj.exe:
	link $<

.cpp.obj:
	cl /EHsc /c $<

clean:
	del *.obj *.exe
