// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* TypeChecking:
 * 
 * Cloneable :- Cloneable[] => Cloneable[] := Cloneable[][]
 */

public class J2_6_Assignable_MultiArray {
    public J2_6_Assignable_MultiArray() {}
    
    public static int test() {
	Cloneable[][] e = null;
	Cloneable[] f = e;
	return 123;
    }
}