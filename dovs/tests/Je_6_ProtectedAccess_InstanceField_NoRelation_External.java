// JOOS1:TYPE_CHECKING,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
// JOOS2:TYPE_CHECKING,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
// JAVAC:UNKNOWN
// 
/* TypeChecking:
 * 
 * Test for Protected Access
 * 
 * accessibleContext has protected access on JComponent (where it is declared)
 * and the class is not a subtype of JComponent nor is it a supertype of JButton
 * (See JLS 6.6.2) 
 */
import javax.accessibility.*;
import javax.swing.*;

public class Je_6_ProtectedAccess_InstanceField_NoRelation_External {
    public Je_6_ProtectedAccess_InstanceField_NoRelation_External() {}
    
    public static int test() {
	JButton button = new JButton();
	AccessibleContext ac = button.accessibleContext;
	return 123;
    }
}