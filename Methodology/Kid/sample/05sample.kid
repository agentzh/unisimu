# 05sample.kid
#	  Page 182, non-disjoint rules
#
#		(x>0 -> (x>y -> z:=x | x<y -> z:=y) |
#		 y>0 -> (x<y -> z:=x | x>y -> z:=y))

if (x > 0) {
    if (x > y) z := x
    if (x < y) z := y
}
if (y > 0) {
    if (x < y) z := x
    if (x > y) z := y
}
