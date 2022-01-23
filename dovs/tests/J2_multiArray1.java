// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING
public class J2_multiArray1 {
    public J2_multiArray1() {
    }

    public static int test() {
        int[][][] a = new int[2][][];
        return 123;
    }
}
