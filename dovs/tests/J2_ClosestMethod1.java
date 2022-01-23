// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_CLOSEST_MATCH_OVERLOADING
public class J2_ClosestMethod1 {
	public J2_ClosestMethod1 (){}
	public void a(Object o) {
		a(null);
	}
	public void a(String s) {

	}
	public static int test() { return 123; }
}
