// TYPE_LINKING
/**
 * TypeLinking:
 * - All classes implicitly import java.lang.*, even in the presence
 * of other import-on-demand declarations.  
 */
import java.lang.ref.*;

public class J1_3_ImportOnDemand_DefaultImportInPresenceOfOtherImport{
    
    public J1_3_ImportOnDemand_DefaultImportInPresenceOfOtherImport(){}

    public static int test(){
	return new Integer(123).intValue();
    }

}
