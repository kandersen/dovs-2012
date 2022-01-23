// JOOS1: PARSER_WEEDER,JOOS1_MULTI_ARRAY,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING,ASSIGN_TYPE
// JAVAC:UNKNOWN
/* TypeChecking:
 * 
 * Closest Method Match: Cloneable[][] is assignable to both Object[] and Cloneable[]
 * and void method(Cloneable[][] e) is therefore the closest matching method.
 * 
 */

public class Je_16_ClosestMatch_Array {
    public Je_16_ClosestMatch_Array() {}
    
    public static int method(Object[] e) { return 123; }
    public static void method(Cloneable[][] e) {}
    public static int method(Cloneable[] e) { return 123; }
    
    public static int test() {
	Cloneable[][] c = null;
	return method(c);
    }
}