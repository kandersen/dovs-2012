//DISAMBIGUATION

public class J1_5_ForwardReference_Method {
	public J1_5_ForwardReference_Method() {
		int b = a; // not a forward reference
	}
	
	public int a = 0; 
	
	public static int test() {
		return 123;
	}
}