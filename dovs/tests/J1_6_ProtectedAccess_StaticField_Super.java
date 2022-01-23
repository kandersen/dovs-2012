//TYPE_CHECKING

/* TypeChecking:
 * 
 * Test for Protected Access
 */ 

public class J1_6_ProtectedAccess_StaticField_Super extends java.io.PipedInputStream {
	public J1_6_ProtectedAccess_StaticField_Super() {
/*		
 * static field access through superclass
 * => OK, since the current type J1_6_ProtectedAccess_StaticField_Super is a subclass of the declaring type java.io.PipedInputStream (6.6.2.1)
 */
		int pipe_size = java.io.PipedInputStream.PIPE_SIZE;
	}
	
	public static int test() {
		return 123;
	}
}