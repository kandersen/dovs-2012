// TYPE_CHECKING
// JOOS1: JOOS1_IMPLICIT_THIS_CLASS_STATIC_METHOD
public class J2_implicitthisclassforstaticmethods {

    public J2_implicitthisclassforstaticmethods() {}

    public static int m1() {
	return 123;
    }

    public int m2() {
	return m1();
    }
    
    public static int test() {
	return new J2_implicitthisclassforstaticmethods().m2();
    }

}
