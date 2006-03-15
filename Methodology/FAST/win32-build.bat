@echo off

perl Makefile.PL
nmake
nmake install
nmake dist
cp -v FAST*.tar.gz win32/src
call pp -x -p -o fast.par script/fast.pl
call unzip -o -d tmp fast.par
del fast.par
cp -v tmp/lib win32/site -r
type blib\script\fast.bat | perl -n -e "s/^perl -x -S/fastperl -x -S/;print" > win32\bin\fast.bat
cd samples
make
cd ..
cp -v samples win32 -r
cd samples
make clean
cd ..
nmake veryclean
