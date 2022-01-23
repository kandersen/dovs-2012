// JOOS1: PARSER_WEEDER,JOOS1_THIS_CALL,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* TypeChecking:
 *	(Joos 2) If the static type of the expression in a throw
 *	statement is a checked exception E2, then the current method
 *	or constructor must declare an exception E1 in its throws
 *	clause such that E2 is a subclass of E1.
 */
import java.io.IOException;

public class J2_ThisException {
	
	public J2_ThisException(int i) throws IOException {
		
	}
	
	public J2_ThisException() throws Exception {
		this(100);
	}
	
	public static int test() {
		return 123;
	}
}
