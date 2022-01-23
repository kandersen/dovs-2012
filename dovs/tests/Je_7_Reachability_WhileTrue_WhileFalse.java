// JOOS1:REACHABILITY,UNREACHABLE_STATEMENT
// JOOS2:REACHABILITY,UNREACHABLE_STATEMENT
// JAVAC:UNKNOWN

public class Je_7_Reachability_WhileTrue_WhileFalse {
	public Je_7_Reachability_WhileTrue_WhileFalse() {}
	
	public static int method() {
		while (true) while (false) {}

	}
	
	public static int test() {
		return 123;
	}
}