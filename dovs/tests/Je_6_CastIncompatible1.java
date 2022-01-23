//JOOS1:TYPE_CHECKING,INVALID_CAST
//JOOS2:TYPE_CHECKING,INVALID_CAST
//JAVAC:

/**
 * TypeChecking:
 * -  either instanceof type must be assignable to expression type 
 *    or expression type must be assignable to instanceof type 
 */
public class Je_6_CastIncompatible1 {
	public Je_6_CastIncompatible1() {}
	
	public static int test() {
		Integer i = new Integer(123);
		return (int)i;
	}
}
