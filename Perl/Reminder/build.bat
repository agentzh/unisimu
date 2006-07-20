@echo off

call pl2bat reminder.pl msgbox.pl
copy reminder.bat E:\perl\bin\
copy msgbox.bat E:\perl\bin\
del reminder.bat msgbox.bat
