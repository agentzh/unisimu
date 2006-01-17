@echo off

call pl2bat sniffer.pl myproxy.pl
copy sniffer.bat E:\perl\bin\
copy myproxy.bat E:\perl\bin\
copy myproxy.pl E:\perl\bin\
del sniffer.bat myproxy.bat
