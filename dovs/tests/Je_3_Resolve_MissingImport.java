// TYPE_LINKING
// JOOS1:TYPE_LINKING,UNRESOLVED_TYPE
// JOOS2:TYPE_LINKING,UNRESOLVED_TYPE
// JAVAC:UNKNOWN
// 
/**
 * Typelinking:
 * -Check that all types actually resolve to defined types in the
 * program or the class library.
 */
public class Je_3_Resolve_MissingImport {

    public Je_3_Resolve_MissingImport() { }

    public static int test() {
	List l = null;
	return 123;
    }
}
