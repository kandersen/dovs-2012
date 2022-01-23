// JOOS1:TYPE_CHECKING
// JOOS2:TYPE_CHECKING
// JAVAC:
/* TypeChecking:
 * Check that the return type of any called method is a valid Joos type (can be
 * checked using the isJoosType method on the type).
 */

public class J1_6_MethodInvocation_NonJoos_ReturnType extends java.awt.Point {
    public J1_6_MethodInvocation_NonJoos_ReturnType() {}
    
    public int getX(int x) {
    	return x;
    }
    
    public static int test() {
		J1_6_MethodInvocation_NonJoos_ReturnType o = new J1_6_MethodInvocation_NonJoos_ReturnType();
		//o.getX(); // return type double (inherited from java.awt.Point)
		return o.getX(123); // ok to call this
    }
}