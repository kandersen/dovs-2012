// JOOS1:DISAMBIGUATION,TYPE_CHECKING,NONSTATIC_FIELD_LINKED_AS_STATIC,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
// JOOS2:DISAMBIGUATION,TYPE_CHECKING,NONSTATIC_FIELD_LINKED_AS_STATIC,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
// JAVAC:UNKNOWN
// 
/* Disambiguation:
 * - A non-static method invocation (ANonstaticInvoke) whose receiver is
 *   a static field access (AStaticFieldLvalue) with a base type resolved
 *   from a prefix of the ambiguous child of the invoke node enclosed in
 *   zero or more non-static field accesses (ANonstaticFieldLvalue).
 *
 */
/**
 * Typecheck:
 * - Check that fields and methods linked as static are actually
 * static, and that those linked as non-static are actually
 * non-static.
 */

public class Je_5_AmbiguousName_PrefixType {
    public Je_5_AmbiguousName_PrefixType() {}
    
    
    public static int test() {
    	// java.lang.System is a type, 
    	// java.lang.System.out is static field of type PrintStream
    	// java.lang.System.out.out is a nonstatic field on PrintStream with protected access
        java.io.OutputStream o = java.lang.System.out.out; 
        return 123;
    }
}