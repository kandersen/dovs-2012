// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* 
 *	(Joos 1) Parser/Weeder:
 *	    No multi-dimensional array allowed.
 *	(Joos 2) TypeChecking:
 *	    Check assignment compatibility of multi-dimensional arrays.
 */

public class J2_16_Assignable_MultiArray {

    public J2_16_Assignable_MultiArray () {}

    public static int test() {
	int[][] is = null;
	is = new int[2][5];
	Object o = is;
	Cloneable c = is;
	java.io.Serializable se1 = is;	
        return 123;
    }

}
