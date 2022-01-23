// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING,CODE_GENERATION
public class J2_FinalField_ProtectedAccess {
    protected final int a = 123;
    
    public J2_FinalField_ProtectedAccess() {}
    
    public int a() {
        return a;
    }
    
    public static int test() {
        return new J2_FinalField_ProtectedAccess().a;
    }
}