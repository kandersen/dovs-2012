// TYPE_LINKING
/**
 * TypeLinking:
 * - Testing whether multiple single-type imports from the same package
 * is taken into account.
 */

import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;

public class J1_3_SingleTypeImport_MultipleFromSamePackage {
    public J1_3_SingleTypeImport_MultipleFromSamePackage() {}
    
    public static int test() {
	Set s = new HashSet();
	return 123;
    }
}