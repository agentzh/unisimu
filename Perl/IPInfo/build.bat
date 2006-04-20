@echo off

call pl2bat ipinfo.pl
copy ipinfo.bat E:\perl\bin\
del ipinfo.bat
