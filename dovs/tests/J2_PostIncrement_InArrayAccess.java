// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_PostIncrement_InArrayAccess {
	
	public J2_PostIncrement_InArrayAccess() {}
	
	public static int test() {
		int[] tmp = new int[5];
		tmp[1] = 123;
		int i = 1;
		return tmp[i++];
	}
}