package foo;

public class Bar {
	public Bar() {}
	
	public static int method() {
		return Main.field; // Main is not visible
	}
}
