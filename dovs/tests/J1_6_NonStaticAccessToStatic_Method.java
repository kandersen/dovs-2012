// JOOS1:TYPE_CHECKING
// JOOS2:TYPE_CHECKING
// JAVAC:
/**
 * Typecheck:
 * - Check that fields and methods linked as static are actually
 * static, and that those linked as non-static are actually
 * non-static.
 */
public class J1_6_NonStaticAccessToStatic_Method {

    public J1_6_NonStaticAccessToStatic_Method() {}
    
    public int m(int x) {
    	return x;
    }
    
    public static int m(String x) {
    	return 123;
    }

    public static int test() {
		J1_6_NonStaticAccessToStatic_Method o = new J1_6_NonStaticAccessToStatic_Method();
		return o.m(123); // nonstatic method invoked
    }
    
}
