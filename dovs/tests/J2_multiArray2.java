// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING
public class J2_multiArray2 {
    public J2_multiArray2() {
    }

    public static int test() {
        int[][][][][][] a = new int[1][2][3][4][5][6];
        return 123;
    }
}
