@echo off

call tpage ../template/parser.pl.tt > parser.pl
echo parser.pl
diff parser.pl ~parser.pl

echo 01test
perl parser.pl 01test > 01out
diff 01out ~01out

echo 02test
perl parser.pl 02test > 02out
diff 02out ~02out

echo 03test
perl parser.pl 03test > 03out
diff 03out ~03out

echo 04test
perl parser.pl 04test > 04out
diff 04out ~04out
