// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_PreDecrement_InNegate {
	
	public J2_PreDecrement_InNegate() {}
	
	public static int test() {
		int i = 124;
		int j = - --i;
		return -j;
	}

}
