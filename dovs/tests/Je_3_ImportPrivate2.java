// TYPE_LINKING
// JOOS1:TYPE_LINKING,UNRESOLVED_TYPE
// JOOS2:TYPE_LINKING,UNRESOLVED_TYPE
// JAVAC:UNKNOWN
// 
public class Je_3_ImportPrivate2 {
    /* java.awt.JobAttributes implicitly includes the private class java.awt.AttributeValue
     * but still java.awt.AttributeValue is private in java.awt and is therefore not accessible
     */
    
    
    public Je_3_ImportPrivate2() {
	java.awt.JobAttributes ja = null;
	java.awt.AttributeValue av = null;
    }
    
    public static int test() {
	return 123;
    }
}