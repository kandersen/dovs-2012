// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: CODE_GENERATION
public class J2_PrePostIncrement_InBinop {
    public J2_PrePostIncrement_InBinop() {}
    public static int test() {
	int foo = 39;
	foo++;
	return (++foo + 2*foo++);
    }
}
