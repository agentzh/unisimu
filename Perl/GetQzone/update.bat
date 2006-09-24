@echo off

set QQ_NUMBER=11854905

perl getqzone.pl %QQ_NUMBER%
perl plotqzone.pl %QQ_NUMBER%
perl archive.pl

pause