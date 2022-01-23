//JOOS1:TYPE_CHECKING,INVALID_CAST
//JOOS2:TYPE_CHECKING,INVALID_CAST
//JAVAC:UNKNOWN

/**
 * TypeChecking:
 * -  either instanceof type must be assignable to expression type 
 *    or expression type must be assignable to instanceof type 
 */
public class Je_6_CastIncompatible2 {
	public Je_6_CastIncompatible2() {}
	
	public static int test() {
		int i = 123;
		if ((boolean)i) {
			return 42;
		} else {
			return 123;
		}
	}
}
