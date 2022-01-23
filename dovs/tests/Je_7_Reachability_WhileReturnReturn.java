// JOOS1:REACHABILITY,UNREACHABLE_STATEMENT
// JOOS2:REACHABILITY,UNREACHABLE_STATEMENT
// JAVAC:UNKNOWN

public class Je_7_Reachability_WhileReturnReturn {
	public Je_7_Reachability_WhileReturnReturn() {}
	
	public static int method(boolean b) {
		while (b) {
			return 123;
			return 42;
		}
		return 87;
	}
	
	public static int test() {
		return Je_7_Reachability_WhileReturnReturn.method(true);
	}
}