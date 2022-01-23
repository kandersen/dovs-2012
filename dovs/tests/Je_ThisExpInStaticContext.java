// JOOS1: TYPE_CHECKING,THIS_IN_STATIC_CONTEXT
// JOOS2: TYPE_CHECKING,THIS_IN_STATIC_CONTEXT
// JAVAC: UNKNOWN
public class Je_ThisExpInStaticContext {
	public Je_ThisExpInStaticContext() {}
	
	public static int test() {
		Object obj = this;
		return 123;
	}
}