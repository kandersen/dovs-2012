// JOOS1:PARSER_WEEDER,PARSER_EXCEPTION,JOOS1_FINAL_FIELD_DECLARATION
// JOOS2:CODE_GENERATION
public class J2_A_FieldInitialization_Final_Before_Final {
	public final int b = this.a; // b = 22
	public final int a = 22; // a = 22 <- canonicalized javac behaviour (not JLS): constant nonstatic final fields are initialized first

	public J2_A_FieldInitialization_Final_Before_Final() {}
	
	public static int test() {
		J2_A_FieldInitialization_Final_Before_Final j = new J2_A_FieldInitialization_Final_Before_Final();
		return j.a + j.b + 79;
	}
}