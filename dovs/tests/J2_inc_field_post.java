// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,DISAMBIGUATION,CODE_GENERATION
public class J2_inc_field_post {
	public int x = 122;
	public J2_inc_field_post() { }
	public static int test() {
		J2_inc_field_post foo = new J2_inc_field_post();
		foo.x++;
		return foo.x++;
	}
}
