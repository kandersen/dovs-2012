//HIERARCHY

/* Hierarchy:
 * A class or interface must not inherit two different fields with the 
 * same name (Joos restriction, well-formedness constraint 11). 
 * (errorInheritedFieldClash)
 *
 * The above does NOT apply since .value is declared here
 */
public class J1_4_DeclaredFieldHidesInherited implements org.omg.CORBA.ARG_IN {
	public String value; // hides org.omg.CORBA.ARG_IN.value
	
	public J1_4_DeclaredFieldHidesInherited() {}
	
	public static int test() {
		return 123;
	}
}
	