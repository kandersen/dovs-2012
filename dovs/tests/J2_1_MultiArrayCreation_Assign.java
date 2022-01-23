// PARSER_WEEDER
// JOOS1: JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
/**
 * Tests if multidimensional arrays are parsed properly.
 */
public class J2_1_MultiArrayCreation_Assign {

    public J2_1_MultiArrayCreation_Assign(){}

    public static int test(){
	Object a = new J2_1_MultiArrayCreation_Assign[5][2];
	if (a instanceof J2_1_MultiArrayCreation_Assign[][]){
	    return 123;
	}
	return 42;	
    }

}
