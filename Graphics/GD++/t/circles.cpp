//: circles.cpp
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-10 2006-04-10

#include <gdpp.h>
#include <math.h>

const double PI        = 3.1415926;
const int    MAX_DEPTH = 5;
double R_scale = 0.5;
double D_scale = 0.2;
int nelems = 10;

void draw_circles (GD::Simple img, double x0, double y0, double D, int depth = 1) {
    if (depth == MAX_DEPTH) return;
    double R = D * R_scale;
    img.moveTo(x0, y0);
    img.ellipse(2 * R, 2 * R);
    double delta_angle = 2 * PI / nelems;
    for (int i = 1; i <= nelems; i++) {
        double angle = delta_angle * i;
        double x = D * cos(angle) + x0;
        double y = D * sin(angle) + y0;
        draw_circles(img, x, y, D * D_scale, depth+1);
    }
}

int main() {
    int width  = 600;
    int height = 600;

    GD::Simple img(width, height);
    img.bgcolor("white");
    img.fgcolor("red");

    draw_circles(img, 0.5*width, 0.5*height, 0.33*width);

    img.as_png("circles.png");
    return 0;
}
