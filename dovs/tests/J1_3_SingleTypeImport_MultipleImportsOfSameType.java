// TYPE_LINKING
/**
 * TypeLinking:
 * - Singletype imports may clash, if the names refer to the same type.
 */

import java.io.File;
import java.io.File;

public class J1_3_SingleTypeImport_MultipleImportsOfSameType{

    public J1_3_SingleTypeImport_MultipleImportsOfSameType(){}

    public static int test(){
	return 123;
    }

}
