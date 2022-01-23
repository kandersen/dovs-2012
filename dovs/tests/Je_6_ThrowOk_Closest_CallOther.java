// JOOS1:TYPE_CHECKING,ILLEGAL_THROWS
// JOOS2:TYPE_CHECKING,ILLEGAL_THROWS
// JAVAC:UNKNOWN

public class Je_6_ThrowOk_Closest_CallOther {
	public Je_6_ThrowOk_Closest_CallOther() {}
	
	public int method(Object a) {
		return 42;
	}
	
	public int method(String b) throws java.io.IOException {
		return 123;
	}
	
	public static int test() {
		Je_6_ThrowOk_Closest_CallOther j = new Je_6_ThrowOk_Closest_CallOther();
		return j.method("");
	}
}