@echo off

perl test_lex.pl tmp > tmp1
perl test_tiny.pl tmp > tmp2
perl gentc.pl tmp > tmp3

perl test_tiny3.pl tmp > tmp4 2>&1

copy  tmp+tmp1+tmp2+tmp3+tmp4 report.txt
@echo report generating...


ep report.txt
pause

