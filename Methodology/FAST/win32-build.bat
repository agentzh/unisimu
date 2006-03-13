@echo off

call pp -x -p -o fast.par script/fast.pl
call unzip -o -d tmp fast.par
cp -v tmp/lib win32/site -r
