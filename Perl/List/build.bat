@echo off

call pl2bat list.pl
copy list.bat E:\perl\bin\
del list.bat
