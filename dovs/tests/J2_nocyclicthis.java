// JOOS1: PARSER_WEEDER,JOOS1_THIS_CALL,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* 
 * We run into the parameterless constructor twice. 
 * This will be rejected if you just mark each ctor as visited.
 */
public class J2_nocyclicthis {
	public J2_nocyclicthis() {
		this(1);
	}
	public J2_nocyclicthis(int foo) {
		this(true);
	}
	public J2_nocyclicthis(boolean foo) {
		
	}
	public J2_nocyclicthis(int foo, int bar) {
		this();
	}

	public static int test() { return 123; }
}
