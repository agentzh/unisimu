//: line.cpp
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-10 2006-04-10

#include <gdpp.h>

int main() {
    GD::Simple img(40, 50);
    img.bgcolor("white");
    img.fgcolor("red");

    img.moveTo(0, 0);
    img.lineTo(20, 30);

    img.as_png("line.png");
    return 0;
}
