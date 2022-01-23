// RESOURCES

public abstract class J1_9_BetterOffsets_Formals_AbstractBefore {
	public J1_9_BetterOffsets_Formals_AbstractBefore() {}
	
	
	
	public abstract int methodA(int a, int b, int c);
	
	public static int methodB(int a, int b, int c) {
		return a+b+c;
	}
	
	public static int test() {
		return J1_9_BetterOffsets_Formals_AbstractBefore.methodB(31,41,51);
	}
}
