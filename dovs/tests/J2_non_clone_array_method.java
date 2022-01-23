// TYPE_CHECKING
// JOOS1: JOOS1_ARRAY_METHOD_CALL,NO_MATCHING_METHOD_FOUND
public class J2_non_clone_array_method {

    public J2_non_clone_array_method() {}

    public static int test() {
	int[] a = new int[2];
	a.toString();
	return 123;
    }
}
