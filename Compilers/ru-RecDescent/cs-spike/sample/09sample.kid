# 09sample.kid
#	Page 184, Example 6.3
#
#	  (x>y -> x,y:=x+abs(y),x-abs(y) |
#	   x<y -> x,y:=y+abs(x),y-abs(x))
#	 =>
#	   x,y:=max(x,y)min(x,y)
#	   x,y:=max(x-y,x+y),min(x-y,x+y)
#	   x,y:=max(x,y),min(x,y)

if (x>y) {
    _a:=x+abs(y); _b:=x-abs(y);
    x:=_a; y:=_b;
} else if (x<y) {
    _a:=y+abs(x); _b:=y-abs(x);
    x:=_a; y:=_b;
}

proc abs (x) {
    if (x>=0) abs:=x;
    else      abs:=-x;
}
