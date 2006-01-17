RM_F = perl -MExtUtils::Command -e rm_f
PERL_BIN = E:\perl\bin

all: copier.bat

copier.bat: copier.pl
	pl2bat copier.pl

install: all
	copy copier.bat $(PERL_BIN)\ 
	copy dout.bat $(PERL_BIN)\ 
	copy din.bat $(PERL_BIN)\ 
	copy aout.bat $(PERL_BIN)\ 
	copy ain.bat $(PERL_BIN)\ 

clean::
	$(RM_F) copier.bat
