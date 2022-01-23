//JOOS1:TYPE_CHECKING,INVALID_INSTANCEOF
//JOOS2:TYPE_CHECKING,INVALID_INSTANCEOF
//JAVAC:UNKNOWN

/**
 * TypeChecking:
 * -  either the instanceof type must be assignable to the expression type 
 *    or the expression type must be assignable to the instanceof type 
 */
public class Je_6_InstanceIncompatible {
	public Je_6_InstanceIncompatible() {}
	
	public static int test() {
		String s = null;
		if (s instanceof Integer) { // invalid instanceof
			return 42;
		} else {
			return 123;
		}
	}
}
