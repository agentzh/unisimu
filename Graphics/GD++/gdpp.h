//: gdpp.h
//: header for GD++
//: Copyright (c) 2006 Agent Zhang
//: 2006-04-10 2006-04-10

#include <string>

namespace GD {
    using namespace std;

    class Color {
        friend class Simple;
    private:
        Color(int R, int G, int B, string id):
            m_R(R), m_G(G), m_B(B), m_id(id) {}
    public:
        int R() { return m_R; }
        int G() { return m_G; }
        int B() { return m_B; }
    private:
        string id() { return m_id; }

        int m_R;
        int m_G;
        int m_B;
        string m_id;
    };

    class Simple {
    public:
        Simple(int width, int height);

        Color colorAllocate(int R, int G, int B);

        void setPixel(int x, int y, Color color);
        Color getPixel(int x, int y);

        void fgcolor(string color);
        void bgcolor(string color);

        void moveTo(int x, int y);
        void lineTo(int x, int y);
        void ellipse(int width, int height);

        void as_png(std::string fname);
    private:
        string itos (int i);
        string m_id;
    };

    extern bool Debug;
}
