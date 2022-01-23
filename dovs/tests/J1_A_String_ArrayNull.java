// CODE_GENERATION

/** CodeGeneration:
 *  This testcase tests the implementation of OBJECT_TO_STRING on arrays.
 */
public class J1_A_String_ArrayNull {
	public J1_A_String_ArrayNull() {}
	
  
	public static int test() {
		int[] a = null;
		String s = "" + a;
		return 123;
	}
}
