// TYPE_CHECKING
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
public class J1_6_ThrowOk_Field {
    public J1_6_ThrowOk_Field() {}
    
    public int field = J1_6_ThrowOk_Field.method(); // OK since RuntimeException is not a checked exception
    
    public static int method() throws RuntimeException {
	return 123;
    }
    
    public static int test() {
	J1_6_ThrowOk_Field j = new J1_6_ThrowOk_Field();
	return j.field;
    }
}