// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING,CODE_GENERATION
public class J2_final_field2 {
    protected final int foo = 123;
    public J2_final_field2() {}
    public static int test() { return new J2_final_field2().foo; }

}
