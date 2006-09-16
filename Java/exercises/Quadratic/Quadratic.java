// Quadratic.java

public class Quadratic {
    public static void main(String[] args) {
        if (args.length != 3)
            die("Usage: java Quadratic <a> <b> <c>");
        double a = Double.parseDouble(args[0]);
        double b = Double.parseDouble(args[1]);
        double c = Double.parseDouble(args[2]);
        if (a == 0)
            die("Error: It's not a valid quadratic equation.");

        double delta = b*b - 4*a*c;
        if (delta < 0)
            die("Error: No solution found.");

        double p = -b / (2*a);
        double q = Math.sqrt(delta) / (2*a);
        
        double x1 = p + q;
        double x2 = p - q;
        System.out.println("Solution Found: " + x1 + ", " + x2);
    }
    private static void die(String s) {
        System.err.println(s);
        System.exit(1);
    }
}
