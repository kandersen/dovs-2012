// ENVIRONMENTS
// JOOS1:ENVIRONMENTS,DUPLICATE_VARIABLE
// JOOS2:ENVIRONMENTS,DUPLICATE_VARIABLE
// JAVAC:UNKNOWN
// 
/**
 * Environments:
 * - Check that no two local variables with overlapping scope have the
 * same name.
 */
public class Je_2_Locals_Overlapping_SameLevel {

    public Je_2_Locals_Overlapping_SameLevel() {}

    public static int test() {
	int r = 0;
	int r = 1;
	return 123;
    }
}
