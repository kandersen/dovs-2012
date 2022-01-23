// TYPE_LINKING
// JOOS1:TYPE_LINKING,UNRESOLVED_TYPE
// JOOS2:TYPE_LINKING,UNRESOLVED_TYPE
// JAVAC:UNKNOWN
// 
public class Je_3_ImportPrivate1 {
    /* java.awt.AttributeValue is private in java.awt and is therefore not accessible*/
    
    public Je_3_ImportPrivate1() {
	java.awt.AttributeValue av = null;
    }
    
    public static int test() {
	return 123;
    }
}