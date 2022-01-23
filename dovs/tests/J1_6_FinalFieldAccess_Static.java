// TYPE_CHECKING

import java.awt.Color;

public class J1_6_FinalFieldAccess_Static {
	public J1_6_FinalFieldAccess_Static() {}
	
	public static int test() {
		Color black = null;
		black = Color.black; // Color.black is final
		return 123;
	}
}
		