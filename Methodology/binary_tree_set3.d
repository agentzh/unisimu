//: binary_tree_set3.d
//: D port for binary_tree_set3.pl
//: Copyright (c) 2006 Agent Zhang
//: 2006-03-03 2006-03-04

import std.conv;
import std.stdio;

int main (char[][] args) {
    int n;
    if (args.length !> 1) {
        fprintf(stderr, "No n specified.\n");
        return 1;
    }
    try { n = toInt(args[1]); }
    catch(Exception e) {
        fprintf(stderr, "n is not a valid integer.\n");
    }
    //printf("n = %d\n", n);
    if (n <= 0) {
        printf("\n");
        return 0;
    }

    // (partially) store the set M and also the n minimals:
    int[] M = new int[n];
    M[0] = 1;
    int pL = 0;
    int pR = 0;
    for (int i = 1; i < M.length; i++) {
        int L = L( M[pL] ); int R = R( M[pR] );
        if (L < R) {
            M[i] = L; pL++;
        } else if (R < L) {
            M[i] = R; pR++;
        } else { // R == L
            M[i] = L;
            pL++; pR++;
        }
    }
    //printf("M.len = %d\n", M.length);
    dump_list(M);

    return 0;
}

int L (int x) {
    return 2 * x + 1;
}

int R (int x) {
    return 3 * x + 1;
}

void dump_list (int[] list) {
    for (int i = 0; i < list.length-1; i++)
        printf("%d ", list[i]);
    printf("%d\n", list[length-1]);
}
