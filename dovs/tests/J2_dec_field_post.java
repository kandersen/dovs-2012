// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_dec_field_post {
	public int x = 124;
	public J2_dec_field_post() { }
	public static int test() {
		J2_dec_field_post foo = new J2_dec_field_post();
		foo.x--;
		return foo.x--;
	}
}
