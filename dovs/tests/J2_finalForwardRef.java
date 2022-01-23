// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: DISAMBIGUATION
public class J2_finalForwardRef {
	public int a = b;
	public static final int b = 2;
	
	public J2_finalForwardRef() {
	}
	
	public static int test() {
		return 123;
	}
}
