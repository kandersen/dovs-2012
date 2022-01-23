// JOOS1: PARSER_WEEDER,JOOS1_THROW,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* TypeChecking:
 *	(Joos 2) If the static type of the expression in a throw
 *	statement is a checked exception E2, then the current method
 *	or constructor must declare an exception E1 in its throws
 *	clause such that E2 is a subclass of E1.
 */
import java.io.IOException;
import java.rmi.ConnectIOException;

/* ConnectionIOException is a subclass of IOException */

public class J2_Throw_SubtypeFromStatic {
	public J2_Throw_SubtypeFromStatic() {
		
	}
	public static int test() {
		return 123;
	}	

	public static int foo() throws IOException, IllegalAccessException, UnsupportedOperationException {
		throw new ConnectIOException("foo");
	}
}
