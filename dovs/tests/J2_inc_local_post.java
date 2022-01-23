// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,DISAMBIGUATION,CODE_GENERATION
public class J2_inc_local_post {
	public J2_inc_local_post() { }
	public static int test() {
		int x=122;
		x++;
		return x++;
	}
}
