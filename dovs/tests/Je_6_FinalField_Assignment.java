// JOOS1:TYPE_CHECKING,ASSIGN_TO_FINAL_FIELD
// JOOS2:TYPE_CHECKING,ASSIGN_TO_FINAL_FIELD
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - (Joos 2) A final field must not be assigned to.
 */
public class Je_6_FinalField_Assignment {

    public Je_6_FinalField_Assignment() {}
    
    public static int test() {
        Integer.MAX_VALUE = 0;
        return 123;
    }
}
