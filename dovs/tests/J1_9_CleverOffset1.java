// RESOURCES

public class J1_9_CleverOffset1 {
	public J1_9_CleverOffset1() {}
	
	public static int test() {
		int a = 0;
		int b = 10;
		int c = 13;
		while (a < 10) {
			c = c + b;
			int d = 1;
			c = c + d;
			
			a = a + 1;
		}
		return c;
	}
}