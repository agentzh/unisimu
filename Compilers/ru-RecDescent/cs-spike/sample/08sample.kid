# 08sample.kid
#	Page 183, Example 6.1
#
#	  f=(x:=x)
#	  P:  if x>0 then x:=x-2*x else x:=x+2*abs(x) fi

proc abs (x) {
    if (x>=0) abs:=x;
    else      abs:=-x;
}

if (x>0) x:=x-2*x else x:=x+2*abs(x)
