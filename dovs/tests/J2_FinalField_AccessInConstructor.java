// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_FinalField_AccessInConstructor {
    protected final int a = 123;
    public int b = 42;
    
    public J2_FinalField_AccessInConstructor() {
        b = a;
    }
    
    public static int test() {
        return new J2_FinalField_AccessInConstructor().b;
    }
}