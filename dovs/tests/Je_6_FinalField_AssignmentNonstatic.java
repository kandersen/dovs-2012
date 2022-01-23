// JOOS1:TYPE_CHECKING,ASSIGN_TO_FINAL_FIELD
// JOOS2:TYPE_CHECKING,ASSIGN_TO_FINAL_FIELD
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - A final field must not be assigned to.
 */
public class Je_6_FinalField_AssignmentNonstatic {

    public Je_6_FinalField_AssignmentNonstatic() {}
    
    public void m() {
    	java.awt.font.GlyphJustificationInfo info = null;
    	info.growPriority = 0; // growPriority is final
    }
    
    public static int test() {
        return 123;
    }
}
