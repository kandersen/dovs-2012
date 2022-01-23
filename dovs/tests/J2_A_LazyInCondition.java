// JOOS1:DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2:CODE_GENERATION

public class J2_A_LazyInCondition {
	public J2_A_LazyInCondition() {}
	
	public static int test() {
		boolean a = true;
		boolean b = false;
		boolean c;
		if (!(a && (c = b))) {
			return 123;
		} else {
			if (c) {
				return 42;
			} else {
				return 7;
			}
		}
	}
}