// DISAMBIGUATION
/** Tests long static dot sequences
 */

public class J1_namelinking3 {
    public J1_namelinking3() {}
    public static int test() {
	Object o1 = Integer.TYPE.getPackage();
	String s2 = java.lang.Integer.TYPE.getName();
	return 1*2*3*4*5 + s2.length();
    }
}
