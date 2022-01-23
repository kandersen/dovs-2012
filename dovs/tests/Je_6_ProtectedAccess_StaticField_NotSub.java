// JOOS1:TYPE_CHECKING,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
// JOOS2:TYPE_CHECKING,PROTECTED_MEMBER_ACCESS,NO_MATCHING_METHOD_FOUND
// JAVAC:UNKNOWN

/* TypeChecking:
 * 
 * Test for Protected Access
 */ 

public class Je_6_ProtectedAccess_StaticField_NotSub /*extends java.io.PipedInputStream*/ {
	public Je_6_ProtectedAccess_StaticField_NotSub() {   
    /* static field declared on subclass - access through this class
     * => FAIL, since java.io.PipedInputStream declares PIPE_SIZE and 
     * Je_6_ProtectedAccess_StaticField_Super is not a subclass of java.io.PipedInputStream (6.6.2.1)
     */ 
		int pipe_size = java.io.PipedInputStream.PIPE_SIZE;
	}
	
	public static int test() {
		return 123;
	}
}