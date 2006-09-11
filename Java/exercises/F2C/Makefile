RM_F = perl -MExtUtils::Command -e rm_f
BIN_FILES = F2C.exe F2C.class

all: $(BIN_FILES)

F2C.exe: F2C.hs
	ghc -o $@ $<

F2C.class: F2C.java
	javac $<

run_haskell: F2C.exe run
	$< 105

run_java: F2C.class run
	java F2C 105

run_perl6: f2c.pl run
	pugs $< 105

clean:
	$(RM_F) *.o *.exe *.class *.hi *.plc

run:
