// DISAMBIGUATION
// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,PARSER_EXCEPTION
public class J2_implicitthisclassforstaticfields {

    public J2_implicitthisclassforstaticfields() {}

    protected static int x;

    public int m() {
	x = 123;
	return x;
    }

    public static int test() {
	return new J2_implicitthisclassforstaticfields().m();
    }

}
