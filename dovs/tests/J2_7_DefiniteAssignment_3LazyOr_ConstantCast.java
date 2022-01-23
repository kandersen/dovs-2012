// DEFINITE_ASSIGNMENT
// JOOS1:JOOS1_OMITTED_LOCAL_INITIALIZER

public class J2_7_DefiniteAssignment_3LazyOr_ConstantCast {
	public J2_7_DefiniteAssignment_3LazyOr_ConstantCast() {}
	
	public boolean method() {
		boolean a;
		boolean b=((boolean) false || (boolean) false) || (a=false);
		return a;
	}
	
	public static int test() {
		return 123;
	}
}