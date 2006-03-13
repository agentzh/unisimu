@echo off

copy blib\script\fast.bat win32\bin\ /Y
call pp -x -p -o fast.par script/fast.pl
call unzip -o -d tmp fast.par
cp -v tmp/lib win32/site -r
