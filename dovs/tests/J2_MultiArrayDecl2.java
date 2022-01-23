// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING
public class J2_MultiArrayDecl2{

    public J2_MultiArrayDecl2(){}

    public static int test(){
	    int[][] a;
	    a = new int[5][2];
	    return 123;
    }
}

