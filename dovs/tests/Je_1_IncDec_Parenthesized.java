//PARSER_WEEDER
//JOOS1:PARSER_EXCEPTION
//JOOS2:PARSER_EXCEPTION

/**
 *	A parenthesized expression as an lvalue is
 *  not supported in joos.
 */
public class Je_1_IncDec_Parenthesized {
	public Je_1_IncDec_Parenthesized() {}
	
	public static int test() {
		int i = 122;
		return ++(i);
	}
}