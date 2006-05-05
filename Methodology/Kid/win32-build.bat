@echo off

perl Makefile.PL
nmake /nologo
nmake /nologo install

nmake /nologo dist
cp -v Kid*.tar.gz win32/src

rm -rf tmp
mkdir tmp

call pp -B -c -p -o kid.par  script/kid2pl.pl script/kid2xml.pl script/kid2mpl.pl script/kid2mm.pl script/kid2mms.pl script/kid2kid.pl
unzip -o -d tmp kid.par
cp -vr tmp/lib win32

rm kid.par
rm -rf tmp



type blib\script\kid2pl.bat | perl -n -e "s/^perl -x -S/kidperl -x -S/;print" > win32\bin\kid2pl.bat


type blib\script\kid2xml.bat | perl -n -e "s/^perl -x -S/kidperl -x -S/;print" > win32\bin\kid2xml.bat


type blib\script\kid2mpl.bat | perl -n -e "s/^perl -x -S/kidperl -x -S/;print" > win32\bin\kid2mpl.bat


type blib\script\kid2mm.bat | perl -n -e "s/^perl -x -S/kidperl -x -S/;print" > win32\bin\kid2mm.bat


type blib\script\kid2mms.bat | perl -n -e "s/^perl -x -S/kidperl -x -S/;print" > win32\bin\kid2mms.bat


type blib\script\kid2kid.bat | perl -n -e "s/^perl -x -S/kidperl -x -S/;print" > win32\bin\kid2kid.bat


rem cd sample
rem make
rem cd ..

cp -v sample win32 -r

rem cd sample
rem make clean
rem cd ..

nmake /nologo veryclean
