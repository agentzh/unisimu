
newfile = test_ffset.txt test_LL1.txt test_Pr_table.txt test_tiny1.txt \
	  test_tiny2.txt test_tiny3.txt test_lex.txt
staticfile = ~test_ffset.txt ~test_LL1.txt ~test_Pr_table.txt ~test_tiny1.txt \
	     ~test_tiny2.txt ~test_tiny3.txt ~test_lex.txt

test_ffset.txt : test_ffset.pl
	perl test_ffset.pl > $@ 2>&1
test_LL1.txt : test_LL1.t
	perl test_LL1.t > $@ 2>&1	
test_Pr_table.txt : test_Pr_table.pl
	perl test_Pr_table.pl > $@ 2>&1
test_tiny1.txt : test_tiny.pl tiny      
	perl test_tiny.pl tiny > $@ 2>&1
test_tiny2.txt : test_tiny.pl tiny2
	perl test_tiny.pl tiny2 > $@ 2>&1 
test_tiny3.txt : test_tiny.pl tiny3
	perl test_tiny.pl tiny3 > $@ 2>&1
test_lex.txt : test_lex.pl tiny lex.pm
	perl test_lex.pl > $@ 2>&1
lex.pm : genlexer.pl
	perl genlexer.pl > lex.pm 
test : $(newfile) $(staticfile)
	diff test_ffset.txt test/~test_ffset.txt
	diff test_LL1.txt test/~test_LL1.txt
	diff test_Pr_table.txt test/~test_Pr_table.txt
	diff test_tiny1.txt test/~test_tiny1.txt
	diff test_tiny2.txt test/~test_tiny2.txt
	diff test_tiny3.txt test/~test_tiny3.txt
	diff test_lex.txt test/~test_lex.txt

clean :
	del test*.txt