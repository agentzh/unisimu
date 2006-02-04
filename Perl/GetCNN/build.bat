@echo off

call pl2bat -w getcnn.pl
copy getcnn.bat E:\perl\bin\
copy pgetcnn.bat E:\perl\bin\
del getcnn.bat
