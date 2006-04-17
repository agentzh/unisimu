//: imcmp.cpp
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-17 2006-04-17

#include <string>
#include <stdio.h>
#include <gdpp.h>

using namespace std;

string rgb2name (int R, int G, int B) {
    if (R == 255 && G == 255 && B == 255) {
        return "white";
    }
    if (R == 255 && G == 0 && B == 0) {
        return "red";
    }
    if (R == 0 && G == 0 && B == 0) {
        return "black";
    }
    if (R == 0 && G == 255 && B == 0) {
        return "green";
    }
    if (R == 0 && G == 0 && B == 255) {
        return "blue";
    }
    return "<" + GD::Simple::itos(R) + ", " +
        GD::Simple::itos(G) + ", " +
        GD::Simple::itos(B) + ">";
}

string get_color(GD::Simple im, int x, int y) {
    GD::Color c = im.getPixel(x, y);
    return rgb2name(c.R(), c.G(), c.B());
}

int main(int argc, char* argv[]) {
    if (argc != 2+1) {
        fprintf(stderr, "Usage: imcmp <image1> <image2>\n");
        return 2;
    }

    string file1 = argv[1];
    string file2 = argv[2];

    printf("Coordinates\t\t%s\t\t%s\n", file1.c_str(), file2.c_str());

    GD::Simple im1(file1);
    GD::Simple im2(file2);

    int w1 = im1.width();
    int h1 = im1.height();
    int w2, h2;
    im2.getBounds(w2, h2);

    if (w1 != w2 || h1 != h2) {
        fprintf(
            stderr, 
            "Images %s and %s are of different sizes.\n",
            file1.c_str(), file2.c_str()
        );
        return 1;
    }

    bool diff = false;
    for (int x = 0; x < w1; x++) {
        for (int y = 0; y < h1; y++) {
            string c1 = get_color(im1, x, y);
            string c2 = get_color(im2, x, y);
            if (c1 != c2) {
                printf("(%d, %d)\t\t%s\t\t%s\n",
                    x, y, c1.c_str(), c2.c_str()
                );
                diff = true;
            }
        }
    }

    return diff ? 1 : 0;
}
