// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_FINAL_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: DISAMBIGUATION,CODE_GENERATION
public class J2_finalstaticfield {
	public static final int foo=122;
	public J2_finalstaticfield() { }
	public static int test() { return 1+foo; }
}
