//: Compare.java

import java.util.*;

public class Compare {
    private static final int NELEMS = 3;

    public static void main(String args[]) {
        if (args.length != NELEMS) {
            System.err.println("Usage: java Compare <num> <num> <num>");
            System.exit(1);
        }
        float list[] = new float[NELEMS];
        for (int i = 0; i < NELEMS; i++)
            list[i] = Float.parseFloat(args[i]);
        Arrays.sort(list);
        for (int i = NELEMS - 1; i > -1; i--) {
            print(Float.toString(list[i]));
            if (i != 0) print(" ");
        }
        print("\n");
    }
    private static void print(String s) {
        System.out.print(s);
    }
}
