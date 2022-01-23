// JOOS1:PARSER_WEEDER,PARSER_EXCEPTION,JOOS1_FINAL_FIELD_DECLARATION
// JOOS2:CODE_GENERATION
public class J2_A_FieldInitialization_NonConstant_Before_Final {
	public int b = this.a; // b = 0
	public final int a = new Integer(22).intValue(); // a = 22

	public J2_A_FieldInitialization_NonConstant_Before_Final() {}
	
	public static int test() {
		J2_A_FieldInitialization_NonConstant_Before_Final j = new J2_A_FieldInitialization_NonConstant_Before_Final();
		return j.a + j.b + 101;
	}
}