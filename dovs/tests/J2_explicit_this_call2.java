// JOOS1: PARSER_WEEDER,JOOS1_THIS_CALL,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,TYPE_CHECKING,CODE_GENERATION
public class J2_explicit_this_call2 {
    protected int zyx;
    public J2_explicit_this_call2(int xyz) {zyx=xyz;}

    public J2_explicit_this_call2() {
	this(321);
    }

    public static int test() { return 444-new J2_explicit_this_call2().zyx; }
}
