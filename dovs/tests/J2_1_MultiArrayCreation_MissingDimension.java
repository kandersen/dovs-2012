// PARSER_WEEDER
// JOOS1: JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
/**
 * - This program tests if multidimensional arrays with missing
 * dimensions are allowed
 */
public class J2_1_MultiArrayCreation_MissingDimension {

    public J2_1_MultiArrayCreation_MissingDimension() {}

    public static int test() {
        int[][] a = new int[2][];
	if (a[1] == null){
	    return 123;
	}
	return 0;
    }
}
