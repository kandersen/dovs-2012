// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_dec_local_pre {
	public J2_dec_local_pre() { }
	public static int test() {
		int x=125;
		--x;
		return --x;
	}
}
