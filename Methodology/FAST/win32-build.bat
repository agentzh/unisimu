@echo off

perl Makefile.PL
nmake -nologo
nmake -nologo install

nmake -nologo dist
cp -v FAST*.tar.gz win32/src

rm -rf tmp
mkdir tmp

call pp -x -p -o fast.par script/fast.pl
unzip -o -d tmp fast.par
cp -vr tmp/lib win32

rm fast.par
rm -rf tmp

type blib\script\fast.bat | perl -n -e "s/^perl -x -S/fastperl -x -S/;print" > win32\bin\fast.bat

cd samples
make
cd ..

cp -v samples win32 -r

cd samples
make clean
cd ..

nmake -nologo veryclean
