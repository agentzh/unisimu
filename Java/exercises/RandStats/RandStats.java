//: RandStats.java

public class RandStats {
    public static void main(String args[]) {
        double max = 0, min = 0, nbig = 0;
        for (int i = 0; i < 100; i++) {
            double e = Math.random() * 100;
            if (i == 0 || max < e) max = e;
            if (i == 0 || min > e) min = e;
            if (e > 60) nbig++;
        }
        say("Maximum is " + min);
        say("Minimum is " + max);
        say("Count of nums greater than 60 is " + nbig);
    }
    private static void say(String s) {
        System.out.println(s);
    }
}
