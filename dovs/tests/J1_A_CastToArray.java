// CODE_GENERATION

public class J1_A_CastToArray {
	public J1_A_CastToArray() {}
	
	public static int test() throws ClassCastException {
		Object o = new Integer(123);
		int[] ia = (int[])o;
		return 123;
	}
}