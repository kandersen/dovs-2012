// CODE_GENERATION

public class J1_A_CastToString {
	public J1_A_CastToString() {}
	
	public static int test() throws ClassCastException {
		Object i = new Integer(123);
		String s = (String)i;
		return 123;
	}
}