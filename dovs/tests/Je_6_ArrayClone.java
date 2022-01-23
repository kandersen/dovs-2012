// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_ARRAY_METHOD_CALL
// JOOS2: NO_MATCHING_METHOD_FOUND
// JAVAC: UNKNOWN
public class Je_6_ArrayClone {
	
	public Je_6_ArrayClone() {}
	
	public static int test() {
		int[] s = new int[2];
		s.clone("foo");
		return 123;
	}
}
