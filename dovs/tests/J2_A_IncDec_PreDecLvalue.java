// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
/**
 * Parser/weeder:
 * - Tests the code generation of --.
 */
public class J2_A_IncDec_PreDecLvalue{

    public J2_A_IncDec_PreDecLvalue(){}

    public static int test(){
	int a = 62;
	int b = --a;
	return a + b + 1;
    }

}
