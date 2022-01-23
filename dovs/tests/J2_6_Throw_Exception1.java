// JOOS1: PARSER_WEEDER,JOOS1_THROW,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* TypeChecking:
 *	(Joos 2) If the static type of the expression in a throw
 *	statement is a checked exception E2, then the current method
 *	or constructor must declare an exception E1 in its throws
 *	clause such that E2 is a subclass of E1.
 */
 public class J2_6_Throw_Exception1 {

    public J2_6_Throw_Exception1 () {}

    public void foo() throws Exception {
        throw new Exception("Exterrestial errors occured!");
    }

    public static int test() {
        return 123;
    }

}
