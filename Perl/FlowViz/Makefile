BATCH_FILES = flowviz.bat flowasm.bat

all: $(BATCH_FILES)

.SUFFIXES: .bat .pl

.pl.bat:
	pl2bat $<

install: all
	copy flowviz.bat E:\perl\bin\ /Y
	copy flowasm.bat E:\perl\bin\ /Y

clean:
	del $(BATCH_FILES)
