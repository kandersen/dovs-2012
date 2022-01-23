// CODE_GENERATION

public class J1_A_AssignNull {
	public J1_A_AssignNull() {}
	
	public static int test() {
		Object o = new Object();
		o = null;
		if (o == null) {
			return 123;
		}
		return 42;
	}
}