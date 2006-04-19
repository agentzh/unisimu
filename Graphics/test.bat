@echo off

perl line.pl > line.png
perl line2.pl > line2.png
perl dda.pl > dda.png
perl bresenham.pl > bresenham.png
perl imcmp.pl dda.png line.png
perl imcmp.pl bresenham.png line.png

echo done.
