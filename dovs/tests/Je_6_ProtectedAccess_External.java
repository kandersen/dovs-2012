//JOOS1:TYPE_CHECKING,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
//JOOS2:TYPE_CHECKING,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
//JAVAC:UNKNOWN

import javax.swing.*;

public class Je_6_ProtectedAccess_External {
	public JComponent component;
	
	public Je_6_ProtectedAccess_External() {
		Object o = component.accessibleContext; // accessibleContext is protected on JComponent
	}
	
	public static int test() {
		return 123;
	}
}