//: F2C.java

public class F2C {
    public static void main(String args[]) {
        if (args.length != 1) {
            System.err.println("Usage: java F2C <num>");
            System.exit(1);
        }
        float F = Float.parseFloat(args[0]);
        float C = (float) 5.0 / 9 * (F - 32);
        System.out.println(C);
    }
}
