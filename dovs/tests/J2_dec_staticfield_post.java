// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_dec_staticfield_post {
	public static int x = 124;
	public J2_dec_staticfield_post() { }
	public static int test() {
		J2_dec_staticfield_post.x--;
		return J2_dec_staticfield_post.x--;
	}
}
