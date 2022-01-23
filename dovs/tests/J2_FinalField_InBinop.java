// JOOS1: PARSER_WEEDER,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING,CODE_GENERATION
public class J2_FinalField_InBinop {
	public final int final_field = 23;

	public J2_FinalField_InBinop() { }
	public static int test() {
		J2_FinalField_InBinop foo = new J2_FinalField_InBinop();
		return 100 + foo.final_field; }
}
