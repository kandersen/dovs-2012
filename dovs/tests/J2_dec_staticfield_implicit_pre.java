// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_dec_staticfield_implicit_pre {
	public static int x = 125;
	public J2_dec_staticfield_implicit_pre() { }
	public static int test() {
		--x;
		return --x;
	}
}
