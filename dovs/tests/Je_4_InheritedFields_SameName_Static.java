// JOOS1:HIERARCHY,INHERITED_FIELD_CLASH
// JOOS2:HIERARCHY,INHERITED_FIELD_CLASH
// JAVAC:
/**
 * Hierarchy check:
 * - A class or interface must not inherit two different fields with
 * the same name (Joos restriction, well-formedness constraint 11)
 *
 *           org.omg.CORBA.OMGVMCID         org.omg.CORBA.CTX_RESTRICT_SCOPE
 *	           interface                             interface
 *         declares static int value              declares static int value
 *	               \                                    /
 *		        \                                  /
 *		                     foo
 * 			    Inherits two fields value
 *                            Compile-time error
 *
 */
import org.omg.CORBA.*;

public class Je_4_InheritedFields_SameName_Static implements OMGVMCID, CTX_RESTRICT_SCOPE {

    public Je_4_InheritedFields_SameName_Static () {}

    public static int test() {
        return 123;
    }

}
