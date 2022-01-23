// JOOS1:HIERARCHY,TYPE_CHECKING,NON_THROWABLE_IN_THROWS
// JOOS2:HIERARCHY,TYPE_CHECKING,NON_THROWABLE_IN_THROWS
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - The types in the throws clause of a method or constructor must be
 * subtypes of java.lang.Throwable
 */
public class Je_6_Throws_NotThrowable {

    public Je_6_Throws_NotThrowable () throws Je_6_Throws_NotThrowable {}
    
    public static int test() {
	return 123;
    }
}
