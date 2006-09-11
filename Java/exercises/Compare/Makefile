RM_F = perl -MExtUtils::Command -e rm_f
BIN_FILES = Compare3.exe Compare.class

all: $(BIN_FILES)

Compare3.exe: Compare3.hs
	ghc -o $@ $<

Compare.class: Compare.java
	javac $<

run_haskell: Compare3.exe run
	$< 3.3 3.9 3.5

run_java: Compare.class run
	java Compare 3.3 3.9 3.5

run_perl6: compare.pl run
	pugs $< 3.3 3.9 3.5

clean:
	$(RM_F) *.o *.exe *.class *.hi *.plc

run:
