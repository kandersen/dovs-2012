// DEFINITE_ASSIGNMENT,CODE_GENERATION
// JOOS1: JOOS1_OMITTED_LOCAL_INITIALIZER
public class J2_defAss2 {
	public J2_defAss2() {}
	
	public static int test() {
		int a;
		if (true && (a = 2) == 2) {
			a = a + 121;
		}
		return a;
	}
}
