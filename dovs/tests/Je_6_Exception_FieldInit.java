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
 * For this purpose, field initializers should be treated as declaring
 * no checked exceptions, i.e. a method or constructor that declares
 * any checked exceptions must not be called from a field initializer.
 */
public class Je_6_Exception_FieldInit {

    public int i = Je_6_Exception_FieldInit.test();

    public Je_6_Exception_FieldInit () {}

    public static int test() throws java.io.IOException {
	return 123;
    }

}
