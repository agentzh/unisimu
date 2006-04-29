# 07sample.kid
#	Page 182, non-disjoint rules
#
#	  (x>0 -> z:=max(x,y) | y>0 -> z:=min(x,y))

if (x > 0) {
    z := max(x, y);
} else if (y > 0) {
    z := min(x, y);
}

proc max(x, y) {
    if (x >= y) max := x;
    else        max := y;
}

proc min(x, y) {
    if (x <= y) min := x;
    else        min := y;
}
