// JOOS1: PARSER_WEEDER,JOOS1_THIS_CALL,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* TypeChecking:
 *	Check that all fields, methods and constructors that are to be linked 
 *  as described in the decoration rules are actually present in the 
 *  corresponding class or interface.
 */
public class J2_thisCall {

    public J2_thisCall (int i) {}

    public J2_thisCall (String s) {
	this(123);
    }

    public static int test() {
        return 123;
    }

}
