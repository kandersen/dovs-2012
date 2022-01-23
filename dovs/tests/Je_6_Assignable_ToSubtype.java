// JOOS1:HIERARCHY,TYPE_CHECKING,ASSIGN_TYPE
// JOOS2:HIERARCHY,TYPE_CHECKING,ASSIGN_TYPE
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - Type Thread is not assignable to type Je_6_Assignable_ToSubtype
 */
public class Je_6_Assignable_ToSubtype extends Thread {

    public Je_6_Assignable_ToSubtype(){}

    public static int test() {
	Je_6_Assignable_ToSubtype a;
	a = new Thread();
	return 123;
    }

}
