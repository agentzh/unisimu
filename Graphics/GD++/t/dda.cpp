//: dda.cpp
//: Digital differential analyzer (DDA) routine for
//:   rasterizing a line
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-10 2006-04-19

#include <gdpp.h>
#include <math.h>

void draw_line(GD::Simple im, int x1, int y1, int x2, int y2, GD::Color color) {
    // approximate the line length
    double Length;
    if (abs(x2 - x1) >= abs(y2 - y1)) {
        Length = abs(x2 - x1);
    } else {
        Length = abs(y2 - y1);
    }
    // select the larger of delta_x or delta_y to be one raster unit
    double delta_x = (x2 - x1) / Length;
    double delta_y = (y2 - y1) / Length;
    // round the values rather than truncate, so that center
    // pixel addressing is handled correctly
    double x = x1 + 0.5;
    double y = y1 + 0.5;
    // begin main loop
    int i = 0;
    while (i <= Length) {
        im.setPixel(int(x), int(y), color);
        x += delta_x;
        y += delta_y;
        i++;
    }
}

int main() {
    GD::Simple im(40, 50);
    GD::Color red = im.colorAllocate(255, 0, 0);
    draw_line(im, 0, 0, 20, 30, red);
    im.as_png("dda.png");
    return 0;
}
