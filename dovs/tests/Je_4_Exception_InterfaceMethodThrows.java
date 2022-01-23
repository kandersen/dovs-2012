// JOOS1:HIERARCHY,ILLEGAL_THROWS_IN_REPLACE
// JOOS2:HIERARCHY,ILLEGAL_THROWS_IN_REPLACE
// JAVAC:UNKNOWN
// 
/**
 * Hierarchy check:
 * - If method m2 overrides method m1, then for every checked
 * exception E2 declared in the throws clause of m2, there must exist
 * an exception E1 declared in the throws clause of m1, such that E2
 * is a subclass of E1 (8.4.4, 8.4.6.3, 8.4.6.4, 9.2, 9.4.1,
 * well-formedness constraint 8).
 */
import java.net.*;

public class Je_4_Exception_InterfaceMethodThrows implements SocketOptions {

    public Je_4_Exception_InterfaceMethodThrows() {}
    
    public Object getOption(int optID) throws ProtocolException{
	return new Object();
    }

    public void setOption(int optID, Object value){}

    public static int test() {
	return 123;
    }
}
