// TYPE_CHECKING

public class J1_6_ThrowOk_Closest_CallOther {
	public J1_6_ThrowOk_Closest_CallOther() {}
	
	public int method(Object a) throws java.io.IOException {
		return 42;
	}
	
	public int method(String b) {
		return 123;
	}
	
	public static int test() {
		J1_6_ThrowOk_Closest_CallOther j = new J1_6_ThrowOk_Closest_CallOther();
		return j.method("");
	}
}