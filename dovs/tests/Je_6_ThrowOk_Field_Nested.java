// JOOS1:TYPE_CHECKING,ILLEGAL_THROWS
// JOOS2:TYPE_CHECKING,ILLEGAL_THROWS
// JAVAC:UNKNOWN

public class Je_6_ThrowOk_Field_Nested {
	public Je_6_ThrowOk_Field_Nested() {}
	
	public void foo() throws java.io.FileNotFoundException {}
	
	public Object a;
	public Object b = a = new java.io.FileReader("foo"); // throws FileNotFoundException
	
	public static int test() {
		return 123;
	}
}