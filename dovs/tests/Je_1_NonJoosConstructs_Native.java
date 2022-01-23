// JOOS1:PARSER_WEEDER,PARSER_EXCEPTION
// JOOS2:PARSER_WEEDER,PARSER_EXCEPTION
/**
 * Parser/weeder:
 * - Native modifier not allowed.
 */
public class Je_1_NonJoosConstructs_Native {

    public Je_1_NonJoosConstructs_Native() {}

    public native void m();

    public static int test() {
		return 123;
    }
}
