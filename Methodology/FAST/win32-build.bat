@echo off

call pp -x -p -o fast.par script/fast.pl
call unzip -o -d tmp fast.par
cp -v tmp/lib win32/site -r
type blib\script\fast.bat | perl -n -e "s/^perl -x -S/fastperl -x -S/;print" > win32\bin\fast.bat
