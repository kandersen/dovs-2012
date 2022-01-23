// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,CODE_GENERATION
public class J2_inc_local_pre {
	public J2_inc_local_pre() { }
	public static int test() {
		int x=121;
		++x;
		return ++x;
	}
}
