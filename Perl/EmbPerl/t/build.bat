@echo off

call pl2bat gen_t.pl
copy gen_t.bat E:\perl\bin\
del gen_t.bat
