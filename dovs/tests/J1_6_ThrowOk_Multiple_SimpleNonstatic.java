// TYPE_CHECKING
/* TypeChecking:
 * For every method invocation expression, class instance creation expression, 
 * implicit or explicit super constructor invocation statement or explicit this
 * constructor invocation statement in the program, check that for every checked
 * exception E2 declared in the throws clause of the invoked method or
 * constructor, the current method or constructor declares an exception E1 in
 * its throws clause such that E2 is a subclass of E1.
 */
public class J1_6_ThrowOk_Multiple_SimpleNonstatic {
	public J1_6_ThrowOk_Multiple_SimpleNonstatic() {
		method((Object)null);
	}
	
	public void method() throws java.io.IOException {}
	public void method(Object o) {}
	
	public static int test() {
		return 123;
	}
}