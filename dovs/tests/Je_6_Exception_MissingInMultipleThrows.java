// JOOS1:TYPE_CHECKING,ILLEGAL_THROWS
// JOOS2:TYPE_CHECKING,ILLEGAL_THROWS
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - For every method invocation expression, class instance creation
 * expression, implicit or explicit super constructor invocation
 * statement or explicit this constructor invocation statement in the
 * program, check that for every checked exception E2 declared in the
 * throws clause of the invoked method or constructor, the current
 * method or constructor declares an exception E1 in its throws clause
 * such that E2 is a subclass of E1.
 */
public class Je_6_Exception_MissingInMultipleThrows {

    public Je_6_Exception_MissingInMultipleThrows () throws java.sql.SQLWarning, java.io.IOException { }

    public static int test() throws java.sql.SQLException {
	new Je_6_Exception_MissingInMultipleThrows();
        return 123;
    }

}
