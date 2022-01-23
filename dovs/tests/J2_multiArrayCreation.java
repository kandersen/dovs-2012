// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING,CODE_GENERATION
public class J2_multiArrayCreation {
	public J2_multiArrayCreation() { }
	public static int test() { 
		int[][] a = new int[2][2];
		a[0][0] = 10;
		a[0][1] = 50;
		a[1][0] = 8;
		a[1][1] = 43;
		return a[1][1] + a[0][0] + a[0][1] - a[1][0] + 28;
	}
}
