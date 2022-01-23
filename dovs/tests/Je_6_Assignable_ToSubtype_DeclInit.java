// JOOS1:TYPE_CHECKING,ASSIGN_TYPE
// JOOS2:TYPE_CHECKING,ASSIGN_TYPE
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - Type Thread is not assignable to type
 * Je_6_Assignable_ToSubtype_DeclInit
 */
public class Je_6_Assignable_ToSubtype_DeclInit extends Thread {

    public Je_6_Assignable_ToSubtype_DeclInit(){}
    
    public static int test() {
	Je_6_Assignable_ToSubtype_DeclInit a = new Thread();
	return 123;
    }

}
