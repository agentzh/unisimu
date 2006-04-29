# 06sample.kid
#	 Page 182, non-disjoint rules
#
#	  (x>0 -> z:=max(x,y) | y>0 -> z:=min(x,y))

if (x > 0) {
    if (x >= y) z := x;
    else        z := y;
} else if (y > 0) {
    if (x <= y) z := x;
    else        z := y;
}
