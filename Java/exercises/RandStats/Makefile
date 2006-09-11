RM_F = perl -MExtUtils::Command -e rm_f
BIN_FILES = RandStats.exe RandStats.class

all: $(BIN_FILES)

RandStats.exe: RandStats.hs
	ghc -o $@ $<

RandStats.class: RandStats.java
	javac $<

run_haskell: RandStats.exe run
	$<

run_perl5: rand-stats.p5 run
	perl $<

run_java: RandStats.class run
	java RandStats

run_perl6: rand-stats.pl run
	perl $<

clean:
	$(RM_F) *.o *.exe *.class *.hi *.plc

run:
