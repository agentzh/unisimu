@echo off

perl getqzone.pl %1 && perl plotqzone.pl %1 && perl archive.pl

pause
