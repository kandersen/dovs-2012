// JOOS1:DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2:CODE_GENERATION
// JAVAC:

public class J2_A_DefiniteAssigment_BetterOffset_Hack {
	public J2_A_DefiniteAssigment_BetterOffset_Hack() {}
	
	public static void method() {
		{
			int a;
			a = 0;
		}
		{
			Object b;
			b = null;
		}
	}
	
	public static int test() {
		return 123;
	}
}