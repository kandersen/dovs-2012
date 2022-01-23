// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING,CODE_GENERATION
public class J2_multiarray {
    public J2_multiarray() {}
    protected int[][] x = new int[42][33];
    public static int test() {
	J2_multiarray obj = new J2_multiarray();
	obj.x[17][18] = 123;
	int[][] y = obj.x;
	return y[17][18];
    }
}

