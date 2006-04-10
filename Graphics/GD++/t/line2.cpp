//: line2.cpp
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-10 2006-04-10

#include <gdpp.h>

void draw_line(GD::Simple im, int x_0, int y_0, int x_A, int y_A, GD::Color color) {
    int x = x_0;
    int y = y_0;
    im.setPixel(x, y, color);
    while (x != x_A || y != y_A) {
        int F_M = y * x_A - y_A * x;
        if (F_M < 0) {
            im.setPixel(x, ++y, color);
        } else if (F_M > 0) {
            im.setPixel(++x, y, color);
        } else {
            im.setPixel(++x, ++y, color);
        }
    }
}

int main() {
    GD::Simple im(40, 50);
    GD::Color red = im.colorAllocate(255, 0, 0); 

    draw_line(im, 0, 0, 20, 30, red);

    im.as_png("line2.png");
    return 0;
}
