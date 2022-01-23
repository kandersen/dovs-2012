// TYPE_LINKING
// JOOS1:TYPE_LINKING,NON_EXISTING_PACKAGE,UNRESOLVED_TYPE
// JOOS2:TYPE_LINKING,NON_EXISTING_PACKAGE,UNRESOLVED_TYPE
// JAVAC:UNKNOWN
// 
/**
 * Type linking:
 * - Check that all types actually resolve to defined types in the
 * program or the class library.
 */
import java.Util.Collection;

public class Je_3_SingleTypeImport_NonExistingPackage {

    public Je_3_SingleTypeImport_NonExistingPackage () {}

    public static int test() {
        return 123;
    }

}
