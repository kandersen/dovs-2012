// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: PARSER_WEEDER,CODE_GENERATION
public class J2_incrementanddecrement {
    public J2_incrementanddecrement() {}
    public static int test() {
	int x = 123;
	x++; 
	x--;
	++x; 
	return --x;
    }
}
