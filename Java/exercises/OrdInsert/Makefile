NAME = OrdInsert
RM_F = perl -MExtUtils::Command -e rm_f
BIN_FILES = $(NAME).exe $(NAME).class

all: $(BIN_FILES)

$(NAME).exe: $(NAME).hs
	ghc -o $@ $<



$(NAME).class: $(NAME).java
	javac $<

run_haskell: $(NAME).exe run
	$< 1 2 3 4 5
	$< 5 4 3 2 1
	$< 5 3 4 2 6 -1

run_java: $(NAME).class run
	java $(NAME) 1 2 3 4 5
	java $(NAME) 5 4 3 2 1
	java $(NAME) 5 3 4 2 6 -1

run_perl6: $(NAME).pl run
	pugs $< 1 2 3 4 5
	pugs $< 5 4 3 2 1
	pugs $< 5 3 4 2 6 -1

clean:
	$(RM_F) *.o *.exe *.class *.hi *.plc

run:
