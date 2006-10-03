//: traffic.cpp
//: Example A of Section 1.2
//: Agent2002. All rights reserved.
//: 2005-03-06 2005-03-07

#include <iostream>
#include <string>
#include <vector>
using namespace std;

const int NELEMS = 3;
vector<string> Res;

void genres (const string& buf) {
    if (buf.length() == NELEMS) {
        Res.push_back(buf);
        return;
    }
    genres(buf + "c");
    genres(buf + "s");
}

int main(void) {
    string buf = "";
    genres(buf);
    cout << "Omega = { ";
    int len = Res.size();
    for (int i = 0; i < len; i++) {
        cout << Res[i];
        if (i != len - 1)
            cout << ", ";
    }
    cout << " }\n";
    return 0;
}

/*
Example A

Driving to work, a commuter passes through a sequence of three 
intersections with traffic lights. At each light, she either 
stops, s, or continues, c. The sample space is the set of all 
possible outcomes:

    Omega = { ccc, ccs, css, csc, sss, ssc, scc, scs }
*/
