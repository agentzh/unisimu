
x,y:=max(x,y),min(x,y)
x,y:=max(x-y,x+y),min(x-y,x+y)
x,y:=max(x,y),min(x,y)

proc max(x, y) {
    if (x >= y) max := x;
    else        max := y;
}

proc min(x, y) {
    if (x <= y) min := x;
    else        min := y;
}
