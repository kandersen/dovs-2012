// JOOS1:HIERARCHY,PROTECTED_REPLACE_PUBLIC
// JOOS2:HIERARCHY,PROTECTED_REPLACE_PUBLIC
// JAVAC:UNKNOWN
// 
/**
 * Hierarchy check:
 * - A protected method must not override a public method (8.4.6.3,
 * well-formedness constraint 7).  
 */
public class Je_4_ProtectedOverride_FromInterface implements Comparable {

    public Je_4_ProtectedOverride_FromInterface() {}
    
    protected int compareTo(Object o) {
	return 0;
    }

    public static int test() {
        return 123;
    }

}
