# 03sample.kid
#      Page 182, disjoint rules
#
#      (x>0 -> (y>0 -> z:=x*y | y<0 -> z:=-x*y) |
#       x<0 -> (y>0 -> z:=-x*y | y<0 -> z:=x*y))

if (x>0) {
    if (y>0) {
        z:=x*y
    }
    if (y<0) {
        z:=-x*y
    }
}
if (x<0) {
    if (y>0) {
        z:=-x*y
    }
    if (y<0) {
        z:=x*y
    }
}
