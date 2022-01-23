// JOOS1:TYPE_CHECKING,NON_JOOS_RETURN_TYPE
// JOOS2:TYPE_CHECKING,NON_JOOS_RETURN_TYPE
// JAVAC:
/* TypeChecking:
 * Check that the return type of any called method is a valid Joos type (can be
 * checked using the isJoosType method on the type).
 */

public class Je_6_MethodInvocation_NonJoos_ReturnType_Static {
    public Je_6_MethodInvocation_NonJoos_ReturnType_Static() {}
    
    public static int test() {
		return (int)Long.parseLong("123"); // return type long
    }
}