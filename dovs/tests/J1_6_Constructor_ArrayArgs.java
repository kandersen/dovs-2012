// TYPE_CHECKING
public class J1_6_Constructor_ArrayArgs {
	protected int field;
	
	public J1_6_Constructor_ArrayArgs(Object[] o) {
		field = 42;
	}
	
	public J1_6_Constructor_ArrayArgs(String[] s) {
		field = 123; // this should be called
	}
	
	public static int test() {
		J1_6_Constructor_ArrayArgs j = new J1_6_Constructor_ArrayArgs(new String[0]);
		return j.field;
	}
}