// JOOS1:PARSER_WEEDER,PARSER_EXCEPTION,JOOS1_FINAL_FIELD_DECLARATION
// JOOS2:CODE_GENERATION
public class J2_A_FieldInitialization_NonConstant_Final_Before {
	public final int b = this.a; // b = 0
	public int a = new Integer(22).intValue(); // a = 22

	public J2_A_FieldInitialization_NonConstant_Final_Before() {}
	
	public static int test() {
		J2_A_FieldInitialization_NonConstant_Final_Before j = new J2_A_FieldInitialization_NonConstant_Final_Before();
		return j.a + j.b + 101;
	}
}