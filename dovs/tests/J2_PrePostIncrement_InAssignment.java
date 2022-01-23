// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,JOOS1_STATIC_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_PrePostIncrement_InAssignment {
	
	public static int i = 123;
	
	public J2_PrePostIncrement_InAssignment(){}
	
	public static int test() {
		i++;
		--i;
		int j = i++;
		return j;
	}

}
