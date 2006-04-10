//: gdpp.cpp
//: implementation for GD++
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-10 2006-04-10

#include "gdpp.h"
#include <embperl.h>
#include "eprintf.h"

static Perl::Interp perl;
static int counter = 0;

namespace GD {
    using namespace std;
    using namespace Perl;

    bool Debug = 0;

    Simple::Simple(int width, int height) {
        perl.eval("use GD::Simple;");
        m_id = "$im" + itos( (int)this );
        string w = itos(width);
        string h = itos(height);
        string cmd = m_id + " = GD::Simple->new("
            + w + ", " + h + ");";
        if (Debug)
            weprintf("GD::Simple::Simple: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    Color Simple::colorAllocate(int R, int G, int B) {
        string r = itos(R);
        string g = itos(G);
        string b = itos(B);
        string c = itos(counter++);

        string color_id = "$color_" + c;
        string cmd = color_id + " = " + m_id + "->colorAllocate("
            + r + ", " + g + ", " + b + ");";
        if (Debug)
            weprintf("GD::Simple::colorAllocate: %s\n", cmd.c_str());

        perl.eval(cmd);

        return Color(R, G, B, color_id);
    }

    void Simple::setPixel(int x, int y, Color color) {
        string xx = itos(x);
        string yy = itos(y);
        string cmd = m_id + "->setPixel(" + xx + ", " +
            yy + ", " + color.id() + ");";
        if (Debug)
            weprintf("GD::Simple::setPixel: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    Color Simple::getPixel(int x, int y) {
        string xx = itos(x);
        string yy = itos(y);
        string cmd = "($R, $G, $B) = " + m_id + "->getPixel(" +
            xx + ", " + yy + ");";
        if (Debug)
            weprintf("GD::Simple::getPixel: %s\n", cmd.c_str());

        perl.eval(cmd);

        int R = perl.SV("R");
        int G = perl.SV("G");
        int B = perl.SV("B");
        return colorAllocate(R, G, B);
    }

    void Simple::fgcolor(string color) {
        string cmd = m_id + "->fgcolor('" + color + "');";
        if (Debug)
            weprintf("GD::Simple::fgcolor: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    void Simple::bgcolor(string color) {
        string cmd = m_id + "->bgcolor('" + color + "');";
        if (Debug)
            weprintf("GD::Simple::bgcolor: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    void Simple::moveTo(int x, int y) {
        string xx = itos(x);
        string yy = itos(y);
        string cmd = m_id + "->moveTo(" + xx + ", " +
            yy + ")";
        if (Debug)
            weprintf("GD::Simple::moveTo: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    void Simple::lineTo(int x, int y) {
        string xx = itos(x);
        string yy = itos(y);
        string cmd = m_id + "->lineTo(" + xx + ", " +
            yy + ")";
        if (Debug)
            weprintf("GD::Simple::lineTo: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    void Simple::ellipse(int width, int height) {
        string w = itos(width);
        string h = itos(height);
        string cmd = m_id + "->ellipse(" + w + ", " +
            h + ")";
        if (Debug)
            weprintf("GD::Simple::ellipse: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    void Simple::as_png(std::string fname) {
        string cmd = "open( $out, '> " + fname +
            "' ) or die \"Can't open " + fname +
            " for writing: $!\";\n" + "binmode $out;\n" +
            "print $out (" + m_id + "->png);\n" +
            "close $out;";
        if (Debug)
            weprintf("GD::Simple::as_png: %s\n", cmd.c_str());

        perl.eval(cmd);
    }

    string Simple::itos (int i) {
        SV tmp = i;
        return tmp;
    }
}
