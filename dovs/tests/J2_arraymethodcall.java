// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_ARRAY_METHOD_CALL
public class J2_arraymethodcall {

    public J2_arraymethodcall() {}

    public static int test() {
	int[] x = new int[123];
	int[] y = (int[])x.clone();
	return y.length;
    }

}
