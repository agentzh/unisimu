//: palindrome.d
//: D port for palindrome.pl
//: Copyright (c) 2006 Agent Zhang
//: 2006-03-04 2006-03-04

import std.cstream;
import std.string;

int check(char[] src) {
    char[] rev = src.dup;
    return cmp(rev.reverse, src) == 0;
}

int main () {
    char[] s = readline();
    //dout.writeLine(s);
    if (check(s))
        say("palindrome");
    else if (strip_space(s) && check(s))
        say("combinational palindrome");
    else
        say("not palindrome");
    return 0;
}

char[] readline() {
    const int INIT_SIZE = 1;
    char[] buf = new char[INIT_SIZE];
    char c;
    int i = 0;
    while ((c = din.getc()) != '\n') {
        if (i == buf.length)
            buf.length = buf.length + 1;
        buf[i++] = c;
    }
    return buf;
}

void say(char[] s) {
    dout.writeLine(s);
}

int strip_space (inout char[] s) {
    s = replace(s, " ", "");
    s = replace(s, "\t", "");
    return 1;
}

/*
$_ = <>; chomp;
if (check) { print "palindrome\n" }
elsif (s/\s+//g and check) { print "combinational palindrome\n" }
else { print "not palindrome\n" }

__END__

*/