// JOOS1:TYPE_CHECKING,NO_MATCHING_CONSTRUCTOR_FOUND
// JOOS2:TYPE_CHECKING,NO_MATCHING_CONSTRUCTOR_FOUND
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - Check that all fields, methods and constructors that are to be
 * linked as described in the decoration rules are actually present in
 * the corresponding class or interface. 
 */
public class Je_6_InstantiateNoVisibleConstructor {
	public Je_6_InstantiateNoVisibleConstructor() {}
	
	public static int test() {
		new java.lang.Math(); // no visible constructors
		return 123;
	}
}