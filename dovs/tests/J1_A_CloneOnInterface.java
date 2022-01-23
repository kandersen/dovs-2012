// CODE_GENERATION

import javax.naming.directory.*;

public class J1_A_CloneOnInterface {
	public J1_A_CloneOnInterface() {}
	
	public static int test() {
		Attribute a = new BasicAttribute("foo");
		Object o = a.clone(); // clone() defined on javax.naming.directory.Attribute
		return 123;
	}
}