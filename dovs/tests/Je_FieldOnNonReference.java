// JOOS1: TYPE_CHECKING, FIELD_ON_NON_REFERENCE
// JOOS2: TYPE_CHECKING, FIELD_ON_NON_REFERENCE
// JAVAC: UNKNOWN
public class Je_FieldOnNonReference {
	public Je_FieldOnNonReference() {}
	
	public static int test() {
		int x = 123;
		int y = x.value;
		return x;
	}
}
