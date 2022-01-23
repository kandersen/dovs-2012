// JOOS1:PARSER_WEEDER,PARSER_EXCEPTION,JOOS1_STATIC_FIELD_DECLARATION,JOOS1_FINAL_FIELD_DECLARATION
// JOOS2:CODE_GENERATION
public class J2_A_FieldInitialization_Static_Before_Final {
	public static int b = J2_A_FieldInitialization_Static_Before_Final.a; // b = 22
	public static final int a = 22; // a = 22 <- should be initialized before non-final static fields

	public J2_A_FieldInitialization_Static_Before_Final() {}
	
	public static int test() {
		return J2_A_FieldInitialization_Static_Before_Final.a + J2_A_FieldInitialization_Static_Before_Final.b + 79;
	}
}