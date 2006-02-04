@echo off

call pl2bat svk-ci.pl
copy svk-ci.bat E:\perl\bin\
del svk-ci.bat
