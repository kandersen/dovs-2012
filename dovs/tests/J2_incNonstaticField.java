// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,CODE_GENERATION
/* Test of post- and preupdate of integers. */
public class J2_incNonstaticField { 

    public int i = 121; 

    public J2_incNonstaticField() {} 

    public int inc() {
	i++;
	return ++i;	
    }

    public static int test() { 
	return new J2_incNonstaticField().inc();
    } 
}
