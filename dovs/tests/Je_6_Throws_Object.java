// JOOS1:HIERARCHY,TYPE_CHECKING,NON_THROWABLE_IN_THROWS
// JOOS2:HIERARCHY,TYPE_CHECKING,NON_THROWABLE_IN_THROWS
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * -  The types in the throws clause of a method or constructor must be
 * subtypes of java.lang.Throwable
 */
public class Je_6_Throws_Object{

    public Je_6_Throws_Object(){}

    public static int test() throws Object{ 
	return 123;
    }

}
