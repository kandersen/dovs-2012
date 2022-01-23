// JOOS1:TYPE_CHECKING,ILLEGAL_THROWS
// JOOS2:TYPE_CHECKING,ILLEGAL_THROWS
// JAVAC:UNKNOWN
// 
/* TypeChecking:
 * For every method invocation expression, class instance creation expression, 
 * implicit or explicit super constructor invocation statement or explicit this
 * constructor invocation statement in the program, check that for every checked
 * exception E2 declared in the throws clause of the invoked method or
 * constructor, the current method or constructor declares an exception E1 in
 * its throws clause such that E2 is a subclass of E1.
 * 
 * For this purpose, field initializers should be treated as declaring no
 * checked exceptions, i.e. a method or constructor that declares any checked
 * exceptions must not be called from a field initializer.
 */
public class Je_6_ThrowOk_Field_StaticNested {
	public Je_6_ThrowOk_Field_StaticNested() {}
	
	public static int method() throws java.io.IOException {
		return 122;
	}
	
	public int field = 1 + Je_6_ThrowOk_Field_StaticNested.method(); // error: method() throws java.io.IOException
	
	public static int test() {
		Je_6_ThrowOk_Field_StaticNested o = new Je_6_ThrowOk_Field_StaticNested();
		return o.field;
	}
}