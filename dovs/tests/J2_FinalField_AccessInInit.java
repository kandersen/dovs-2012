// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: DISAMBIGUATION,CODE_GENERATION
public class J2_FinalField_AccessInInit {
    protected final int a = 123;
    public int b = a;
    
    public J2_FinalField_AccessInInit() {}
    
    public static int test() {
        return new J2_FinalField_AccessInInit().b;
    }
}