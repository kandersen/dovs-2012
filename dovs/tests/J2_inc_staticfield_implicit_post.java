// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,JOOS1_STATIC_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,CODE_GENERATION
public class J2_inc_staticfield_implicit_post {
	public static int x = 122;
	public J2_inc_staticfield_implicit_post() { }
	public static int test() {
		x++;
		return x++;
	}
}
