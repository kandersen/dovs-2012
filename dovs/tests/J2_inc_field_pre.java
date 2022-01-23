// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,DISAMBIGUATION,CODE_GENERATION
public class J2_inc_field_pre {
	public int x = 121;
	public J2_inc_field_pre() { }
	public static int test() {
		J2_inc_field_pre foo = new J2_inc_field_pre();
		++foo.x;
		return ++foo.x;
	}
}
