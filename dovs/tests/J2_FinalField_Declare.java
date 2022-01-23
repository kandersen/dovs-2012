// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER
public class J2_FinalField_Declare {

    public J2_FinalField_Declare () {}

    public final int i = 123;

    public static int test() {
        return 123;
    }

}
