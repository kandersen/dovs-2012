// RESOURCES

public class J1_9_BetterOffsets_Formals {
	public J1_9_BetterOffsets_Formals() {}
	
	public int methodA(int a, int b, int c) {
		return a+b+c;
	}
	
	public static int methodB(int a, int b, int c) {
		return a+b+c;
	}
	
	public static int test() {
		J1_9_BetterOffsets_Formals j = new J1_9_BetterOffsets_Formals();
		return j.methodA(10,20,30)+J1_9_BetterOffsets_Formals.methodB(11,21,31);
	}
}
