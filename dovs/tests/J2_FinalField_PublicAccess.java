// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING,CODE_GENERATION
public class J2_FinalField_PublicAccess {
    public J2_FinalField_PublicAccess() {}
    public final int x = 123;
    public static int test() {
	return new J2_FinalField_PublicAccess().x;
    }
}
