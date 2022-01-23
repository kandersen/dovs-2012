// JOOS1: PARSER_WEEDER,JOOS1_INC_DEC,PARSER_EXCEPTION
// JOOS2: DEFINITE_ASSIGNMENT,VARIABLE_MIGHT_NOT_HAVE_BEEN_INITIALIZED
// JAVAC: UNKNOWN
/**
 * Definite Assignment
 * - (Joos 2) A local variable must be initialized before it is used.
 *
 * This testcase tests whether a local lvalue in an inc-dec expression 
 * is treated as a read access (right-hand-side) in the analysis.  
 */
public class Je_18_DefiniteAssignment_InInitializer_IncDec {
	public Je_18_DefiniteAssignment_InInitializer_IncDec() {
		int x = x++;
	}
	public static int test() {
		return 123;
	}
}
