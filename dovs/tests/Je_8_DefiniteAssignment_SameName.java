// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT,VARIABLE_MIGHT_NOT_HAVE_BEEN_INITIALIZED
// JAVAC: UNKNOWN
/**
 * Definite Assignment
 * - (Joos 1) A local variable declaration must have an initializer
 * - (Joos 2) A local variable must be initialized before it is used.
 *
 * This testcase tests whether the implemented notion variable identity 
 * can handle reoccuring names.
 */
public class Je_8_DefiniteAssignment_SameName {

    public Je_8_DefiniteAssignment_SameName() {
		{
			int a = 123;
		}
		int a;
		int b = a;

    }

    public static int test(){
		return 123;
    }

}
