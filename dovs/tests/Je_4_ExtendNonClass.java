// JOOS1:HIERARCHY,EXTENDS_NON_CLASS
// JOOS2:HIERARCHY,EXTENDS_NON_CLASS
// JAVAC:UNKNOWN
// 
/**
 * Hierarchy:
 * - The type mentioned in the extends clause of a class must be a class (8.1.3, simple constraint 1).
 */
public class Je_4_ExtendNonClass extends Cloneable{

    public Je_4_ExtendNonClass(){}

    public static int test(){
	return 123;
    }
}
