RM_F = perl -MExtUtils::Command -e rm_f
BIN_FILES = Quadratic.exe Quadratic.class

all: $(BIN_FILES)

Quadratic.exe: Quadratic.hs
	ghc -o $@ $<

Quadratic.class: Quadratic.java
	javac $<

run_haskell: Quadratic.exe run
	$< 1 1 -6
	$< 1 1 6
	$< 1 -2 1
	$< 0 1 2

run_java: Quadratic.class run
	java Quadratic 1 1 -6
	java Quadratic 1 1 6
	java Quadratic 1 -2 1
	java Quadratic 0 1 2

run_perl6: Quadratic.pl run
	pugs $< 1 1 -6
	pugs $< 1 1 6 
	pugs $< 1 -2 1
	pugs $< 0 1 2 

clean:
	$(RM_F) *.o *.exe *.class *.hi *.plc

run:
