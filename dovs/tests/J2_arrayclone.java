// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_ARRAY_METHOD_CALL
public class J2_arrayclone {

    public J2_arrayclone () {}

    public static int test() {
	Object obj = (new int[42]).clone();
	int[] is = (int[])((int[])obj).clone();
        return 123;
    }

}
