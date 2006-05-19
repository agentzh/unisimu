@echo off

perl lexer.pl test1 > out1
perl lexer.pl test2 > out2

echo. comparing outputs...
diff out1 ~out1
diff out2 ~out2
echo. done.
