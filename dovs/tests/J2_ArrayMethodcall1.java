// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_ARRAY_METHOD_CALL
public class J2_ArrayMethodcall1 {
	
	public J2_ArrayMethodcall1() {}
	
	public static int test() {
		int[] s = new int[2];
		s.toString();
		return 123;
	}
}
