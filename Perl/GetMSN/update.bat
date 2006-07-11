@echo off

set MSN_USER=cherrychuxinyun

perl getmsn.pl %MSN_USER%
perl getmsn++.pl %MSN_USER%
perl plotmsn.pl %MSN_USER%
perl plotmsn.pl %MSN_USER%++
perl archive.pl

pause
