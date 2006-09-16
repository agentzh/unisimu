import java.util.*;

public class OrdInsert {
    public static void main(String args[]) {
        if (args.length == 0) {
            System.err.println("Usage: java OrdInsert <int>+");
            System.exit(1);
        }
        List ls = new LinkedList();
        for (int i = 0; i < args.length; i++)
            ordInsert(ls, Integer.parseInt(args[i]));
        System.out.println(join(" ", ls));
    }
    private static void ordInsert(List ls, int elem) {
        int i = 0;
        for (Iterator<Integer> it = ls.iterator(); it.hasNext(); i++) {
            int curr = (Integer) it.next();
            if (curr < elem) continue;
            ls.add(i, elem);
            return;
        }
        ls.add(i, elem);
    }
    private static String join(String sep, List ls) {
        String str = "";
        boolean firstTime = true;
        for (Iterator it = ls.iterator(); it.hasNext();) {
            if (!firstTime) str += sep;
            else firstTime = false;
            str += it.next().toString();
        }
        return str;
    }
}
