@echo off

call pl2bat reindex.pl
copy reindex.bat E:\perl\bin\
del reindex.bat
