// PARSER_WEEDER
// JOOS1: JOOS1_INC_DEC,PARSER_EXCEPTION
/**
 * Parser/weeder:
 * - Tests the parsing of minus minus when cast.
 */
public class J2_1_IncDec_Cast{
    
    public J2_1_IncDec_Cast(){}

    public static int test(){
	int a = 124;
	int b = (int)--a;
	return b;
    }
}
