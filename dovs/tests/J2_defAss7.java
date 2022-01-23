// DEFINITE_ASSIGNMENT,CODE_GENERATION
// JOOS1: PARSER_WEEDER,JOOS1_THIS_CALL,PARSER_EXCEPTION
public class J2_defAss7 {
	protected int x;
	
	public J2_defAss7() {
		this(3);
	}
	
	public J2_defAss7(int x) {
		this.x = x;
	}

	public static int test() {
		int b;
		if (false) {
			int a = b;
		}
		
		J2_defAss7 a;
		a = new J2_defAss7(b = 60);
		return a.x + b + 3;
	}
}
