// CODE_GENERATION
public class J1_A_FieldInitializer {
	public int field = 123;
	
	public J1_A_FieldInitializer() {}
	
	public static int test() {
		J1_A_FieldInitializer j = new J1_A_FieldInitializer();
		return j.field;
	}
}