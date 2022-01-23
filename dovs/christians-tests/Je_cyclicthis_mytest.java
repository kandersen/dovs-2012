// JOOS1: PARSER_WEEDER,JOOS1_THIS_CALL,PARSER_EXCEPTION
// JOOS2: TYPE_CHECKING
/* 
 * We run into the parameterless constructor twice. 
 * This will be rejected if you just mark each ctor as visited.
 */
public class Je_cyclicthis_mytest {
	public Je_cyclicthis_mytest() {
		this(1);
	}
	public Je_cyclicthis_mytest(int foo) {
		this(true);
	}
	public Je_cyclicthis_mytest(boolean foo) {
		this(1);
	}
	public Je_cyclicthis_mytest(int foo, int bar) {
		this();
	}

	public static int test() { return 123; }
}