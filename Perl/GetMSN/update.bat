@echo off

perl getmsn.pl %1
perl getmsn++.pl %1
perl plotmsn.pl %1
perl plotmsn.pl %1++
perl archive.pl

pause
