// DEFINITE_ASSIGNMENT,CODE_GENERATION
// JOOS1: PARSER_WEEDER,JOOS1_OMITTED_CONSTRUCTOR
public class J2_defasn_lazyor {
    public static boolean method(boolean a) {
	boolean b;
	boolean c = !(a || (b = false) || !a && b);
	return c;
    }

    public static void main(String[] args) {
	System.out.println(method(false));
    }

    public static int test() {
	if (method(false)) {
	    return 123;
	}
	return 7;
    }
} 
