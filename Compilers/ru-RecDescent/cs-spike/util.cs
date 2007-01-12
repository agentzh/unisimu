using System;
using System.Collections;

class Util {
    public static Hashtable NewHash(params object[] pairs) {
		Hashtable h = new Hashtable();
		if (pairs.Length % 2 != 0) {
			Console.Error.WriteLine("Warning: Odd number of arguments for NewHash");
			return h;
		}
		for (int i = 0; i < pairs.Length; i += 2) {
			h.Add(pairs[i], pairs[i+1]);
		}
		return h;
	}
    public static ArrayList NewArray(params object[] elems) {
		return new ArrayList(elems);
	}
	public static string dump_data(object tree, params object[] extra) {
		int level = 0;
		if (extra.Length > 0) level = (int) extra[0];
		string indent = str_mul("  ", level);
		string s;
		if (tree is Hashtable) {
			IDictionaryEnumerator e = ((Hashtable)tree).GetEnumerator();
			s = "\n" + indent + "{\n";
			while (e.MoveNext()) {
				s += indent + "  '" + quote_str(e.Key) + "' => " +
                    dump_data(e.Value, level+1) + ",\n";
			}
			s += indent + "}";
		} 
		else if (tree is ArrayList) {
			s = "\n" + indent + "[\n";
			foreach (object val in (ArrayList)tree) {
				s += indent + "  " + dump_data(val, level+1) + ",\n";
			}
			s += indent + "]";
		}
		else {
			s = "'" + quote_str(tree.ToString()) + "'";
		}
		return s;
	}
	private static string quote_str(object obj) {
		string s = obj.ToString();
		s = s.Replace("\\", "\\\\");
		s = s.Replace("'", "\\'");
		return s;
	}
	private static string str_mul(string s, int times) {
		string res = "";
		for (int i = 0; i < times; i++)
			res += s;
		return res;
	}
}
