// JOOS1:HIERARCHY,REPLACE_FINAL
// JOOS2:HIERARCHY,REPLACE_FINAL
// JAVAC:UNKNOWN
// 
/* JLS 8.4.3.3:
 * It is a compile-time error to attempt to override or hide a final method.
 */

import javax.swing.text.*;

public class Je_4_FinalHide extends Utilities {
	public Je_4_FinalHide() {}
	
    public static int getRowStart(JTextComponent c, int offs) {
    	return 0;
    }
    
    public static int test() {
    	return 123;
    }
}