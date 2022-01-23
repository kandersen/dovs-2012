// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER
public class J2_staticFinal {

    public J2_staticFinal () {}
    
    public static final int i = 123;

    public final int k = 2;

    public static int l = 3;

    public static int test() {
        return i;
    }

}
