//HIERARCHY,DUPLICATE_METHOD
//JAVAC:UNKNOWN

/**
 * Hierarchy:
 * A class or interface must not declare two methods with 
 * the same name and parameter types (8.4, 9.4, well-formedness constraint 2). 	
 */
public class Je_4_DuplicateMethod_NameClashOnReturnType {
	public Je_4_DuplicateMethod_NameClashOnReturnType() {}
	
	public Integer Integer() { return null; }
	public void Integer() {}
	
	public static int test() {
		return 123;
	}
}