// JOOS1:HIERARCHY,IMPLEMENTS_NON_INTERFACE
// JOOS2:HIERARCHY,IMPLEMENTS_NON_INTERFACE
// JAVAC:UNKNOWN
// 
/**
 * Hierarchy:
 * - All types mentioned in the implements clause of a class must be
 * interfaces (8.1.4, simple constraint 2)
 */
public class Je_4_ImplementNonInterface_InterfaceAndClass implements Cloneable, Object{

    public Je_4_ImplementNonInterface_InterfaceAndClass(){}

    public static int test(){
	return 123;
    }

}
