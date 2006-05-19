# lexer.pl

use strict;
use warnings;

local $/;
local $_ = <>;

while (1) {
    if (/\G\s*DIM\b/gc) {
        print "DIM: DIM\n";
    }
    elsif (/\G\s*IF\b/gc) {
        print "IF: IF\n";
    }
    elsif (/\G\s*DO\b/gc) {
        print "DO: DO\n";
    }
    elsif (/\G\s*STOP\b/gc) {
        print "STOP: STOP\n";
    }
    elsif (/\G\s*END\b/gc) {
        print "END: END\n";
    }
    elsif (/\G\s*([A-Za-z_]\w*)\b/gc) {
        print "ID: $1\n";
    }
    elsif (/\G\s*([1-9]\d*)\b/gc) {
        print "NUM: $1\n";
    }
    elsif (/\G\s*=/gc) {
        print "ASSIGN: =\n";
    }
    elsif (/\G\s*\+/gc) {
        print "ADD: +\n";
    }
    elsif (/\G\s*-/gc) {
        print "SUB: -\n";
    }
    elsif (/\G\s*\*\*/gc) {
        print "POWER: **\n";
    }
    elsif (/\G\s*\*/gc) {
        print "MUL: *\n";
    }
    elsif (/\G\s*,/gc) {
        print "COMMA: ,\n";
    }
    elsif (/\G\s*\(/gc) {
        print "LEFT: (\n";
    }
    elsif (/\G\s*\)/gc) {
        print "RIGHT: )\n";
    }
    elsif (/\G\s*(\S+)/gc) {
        print "ERROR: $1\n";
    } else {
        last;
    }
}
