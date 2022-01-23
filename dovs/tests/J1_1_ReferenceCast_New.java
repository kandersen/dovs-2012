//PARSER_WEEDER
public class J1_1_ReferenceCast_New {
	public J1_1_ReferenceCast_New() {
	}
	
	public static int test() {
		Object o = (Object)new Object();
		return 123;
	}
}