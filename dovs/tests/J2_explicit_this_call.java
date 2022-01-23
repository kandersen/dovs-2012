// JOOS1: PARSER_WEEDER,JOOS1_THIS_CALL,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING
public class J2_explicit_this_call {
    public J2_explicit_this_call(int xyz) {}

    public J2_explicit_this_call() {
	this(321);
    }

    public static int test() { return 123; }
}
