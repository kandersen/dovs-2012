// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_dec_local_post {
	public J2_dec_local_post() { }
	public static int test() {
		int x=124;
		x--;
		return x--;
	}
}
