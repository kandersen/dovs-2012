// JOOS1:HIERARCHY,PROTECTED_REPLACE_PUBLIC
// JOOS2:HIERARCHY,PROTECTED_REPLACE_PUBLIC
// JAVAC:UNKNOWN
// 
/**
 * Hierarchy check:
 * - A protected method must not override a public method (8.4.6.3,
 * well-formedness constraint 7). (protected method clone() inherited 
 * from superclass java.lang.Object, but interface javax.naming.Name
 * requires method clone() to be public).
 */
public abstract class Je_4_ProtectedOverride_Exception_Clone 
    implements javax.naming.Name {

    public Je_4_ProtectedOverride_Exception_Clone(){}

    public static int test() {
	return 123;
    }

}
