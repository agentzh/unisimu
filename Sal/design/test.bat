@echo off
perl test_ffset.pl > test_ffset.txt
perl test_LL1.t > test_LL1.txt 2>&1
perl test_Pr_table.pl p2.txt > test_Pr_table.txt
perl test_tiny.pl > test_tiny1.txt 2>&1
perl test_tiny.pl tiny2 > test_tiny2.txt 2>&1
perl test_tiny.pl tiny3 > test_tiny3.txt 2>&1
perl test_lex.pl > test_lex.txt 2>&1
perl gentc.pl > test_gentc.txt 2>&1

echo diff test_ffset.txt test/~test_ffset.txt
diff test_ffset.txt test/~test_ffset.txt

echo diff test_LL1.txt test/~test_LL1.txt
diff test_LL1.txt test/~test_LL1.txt

echo diff test_Pr_table.txt test/~test_Pr_table.txt
diff test_Pr_table.txt test/~test_Pr_table.txt

echo diff test_tiny1.txt test/~test_tiny1.txt
diff test_tiny1.txt test/~test_tiny1.txt

echo diff test_tiny1.txt test/~test_tiny1.txt
diff test_tiny2.txt test/~test_tiny2.txt

echo diff test_tiny1.txt test/~test_tiny1.txt
diff test_tiny3.txt test/~test_tiny3.txt

echo diff test_lex.txt test/~test_lex.txt
diff test_lex.txt test/~test_lex.txt

echo diff test_gentc.txt test/~test_gentc.txt
diff test_gentc.txt test/~test_gentc.txt

del test*.txt
echo test finished!
pause

