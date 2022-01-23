// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING
public class J2_multiArrays {
	public J2_multiArrays() { }
	public static int test() { 
		int[][] a = new int[3][3];
		return 123; 
	}
}
