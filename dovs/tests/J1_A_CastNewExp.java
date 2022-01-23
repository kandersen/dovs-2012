// CODE_GENERATION

import java.util.*;

public class J1_A_CastNewExp {
	public J1_A_CastNewExp() {}
	
	public static int test() throws ClassCastException {
		Stack s = (Stack)new Vector();
		return 123;
	}
}