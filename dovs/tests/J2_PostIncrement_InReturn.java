// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_PostIncrement_InReturn {
    public J2_PostIncrement_InReturn() {}
    public static int test() {
	int foo = 123;
	return (foo++);
    }
}
