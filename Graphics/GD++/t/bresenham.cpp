//: bresenham.cpp
//: Generalized integer Bresenham's line rasterization algorithm
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-19 2006-04-19

#include <gdpp.h>
#include <math.h>

int sign(int value) {
    if (value > 0)
        return 1;
    else if (value == 0) 
        return 0;
    else
        return -1;
}

void draw_line(GD::Simple im, int x1, int y1, int x2, int y2, GD::Color color) {
    int x = x1;
    int y = y1;
    int delta_x = abs(x2 - x1);
    int delta_y = abs(y2 - y1);
    int s1 = sign(x2 - x1);
    int s2 = sign(y2 - y1);
    // interchange delta_x and delta_y, depending on the slope
    //   of the line
    bool Interchange;
    if (delta_y > delta_x) {
        int Temp = delta_x;
        delta_x = delta_y;
        delta_y = Temp;
        Interchange = true;
    }
    // initialize the error term to compensate for a nonzero intercept
    int e = 2 * delta_y - delta_x;
    // begin the main loop
    for (int i = 0; i <= delta_x; i++) {
        im.setPixel(x, y, color);
        while (e > 0) {
            if (Interchange) {
                x += s1;
            } else {
                y += s2;
            }
            e -= 2 * delta_x;
        }
        if (Interchange) {
            y += s2;
        } else {
            x += s1;
        }
        e += 2 * delta_y;
    }
}

int main() {
    GD::Simple im(40, 50);
    GD::Color red = im.colorAllocate(255, 0, 0);
    draw_line(im, 0, 0, 20, 30, red);
    im.as_png("bresenham.png");
    return 0;
}
