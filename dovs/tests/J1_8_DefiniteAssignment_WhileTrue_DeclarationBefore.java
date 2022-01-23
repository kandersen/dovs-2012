// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT
// JAVAC:
/**
 * Definite Assignment:
 * - (Joos 1) A local variable declaration must have an initializer.
 * - (Joos 2) Whenever a local variable is used in any context which
 * is not the direct left-hand side of an assignment, it must be
 * definitely assigned at that point in the program. (a is not 
 * definitely assigned)
 */
public class J1_8_DefiniteAssignment_WhileTrue_DeclarationBefore {

    public J1_8_DefiniteAssignment_WhileTrue_DeclarationBefore() {}

	public static int m() {
		int a;
		boolean b = false;
		while (true || b) {
		}
		// a is definitely assigned here because 
		// it was in scope at the while condition
		return a; 
	}
	
    public static int test() {
    	return 123;
    }
}
