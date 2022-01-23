//DISAMBIGUATION

/** Disambiguation:
 *  Check for correct enclosing of nonstatic fields.
 */

public class J1_5_AmbiguousName_EnclosingFields {
	public J1_5_AmbiguousName_EnclosingFields b;
	public J1_5_AmbiguousName_EnclosingFields c;
	public J1_5_AmbiguousName_EnclosingFields d;
	public J1_5_AmbiguousName_EnclosingFields e;
	public String string;
	
	public J1_5_AmbiguousName_EnclosingFields(String prefix, int level) {
		string = prefix; 
		if (level >= 0) {
			b = new J1_5_AmbiguousName_EnclosingFields(prefix + ".b", level-1);
			c = new J1_5_AmbiguousName_EnclosingFields(prefix + ".c", level-1);
			d = new J1_5_AmbiguousName_EnclosingFields(prefix + ".d", level-1);
			e = new J1_5_AmbiguousName_EnclosingFields(prefix + ".e", level-1);
		}
	}
	
	public static int test() {
		int result = 0;
		J1_5_AmbiguousName_EnclosingFields a = new J1_5_AmbiguousName_EnclosingFields("a",5);
		String a_string = a.string;
		System.out.println(a_string);
		if (a_string.equals((Object)"a")) {
			result = result + 1;
		}
		String ab_string = a.b.string;
		System.out.println(ab_string);
		if (ab_string.equals((Object)"a.b")) {
			result = result + 2;
		}
		String abc_string = a.b.c.string;
		System.out.println(abc_string);
		if (abc_string.equals((Object)"a.b.c")) {
			result = result + 4;
		}
		String abcd_string = a.b.c.d.string;
		System.out.println(abcd_string);
		if (abcd_string.equals((Object)"a.b.c.d")) {
			result = result + 8;
		}
		String abcde_string = a.b.c.d.e.string;
		System.out.println(abcde_string);
		if (abcde_string.equals((Object)"a.b.c.d.e")) {
			result = result + 16;
		}
		String aedceb_string = a.e.d.c.e.b.string;
		System.out.println(aedceb_string);
		if (aedceb_string.equals((Object)"a.e.d.c.e.b")) {
			result = result + 32;
		}
		return result+60;
	}
}