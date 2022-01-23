// DEFINITE_ASSIGNMENT
// JOOS1:JOOS1_OMITTED_LOCAL_INITIALIZER

public class J2_7_DefiniteAssignment_2LazyOr_ConstantCast {
	public J2_7_DefiniteAssignment_2LazyOr_ConstantCast() {}
	
	public boolean method() {
		boolean j;
		boolean i;
		if (((boolean)false) || (i=true));
		return i; 
	}
	
	public static int test() {
		return 123;
	}
}