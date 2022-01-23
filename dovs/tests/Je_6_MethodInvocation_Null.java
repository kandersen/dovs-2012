// JOOS1:TYPE_CHECKING,NON_REFERENCE_RECEIVER
// JOOS2:TYPE_CHECKING,NON_REFERENCE_RECEIVER
// JAVAC:UNKNOWN
/**
 * TypeChecking:
 * - Method invocation on null type not allowed
 */
public class Je_6_MethodInvocation_Null {

    public Je_6_MethodInvocation_Null() {}

    public static int test () {
		null.m();
		return 123;
    }
} 