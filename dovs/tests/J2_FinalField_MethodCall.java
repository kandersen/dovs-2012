// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING,CODE_GENERATION
public class J2_FinalField_MethodCall {
    protected final Object a = new Integer(123);
    
    public J2_FinalField_MethodCall() {}
    
    public Integer a() {
        return (Integer) a;
    }
    
    public static int test() {
        return new J2_FinalField_MethodCall().a().intValue();
    }
}