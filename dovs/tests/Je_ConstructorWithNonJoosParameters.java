// JOOS1: TYPE_CHECKING, NO_MATCHING_CONSTRUCTOR_FOUND, NON_JOOS_PARAMETER_TYPE
// JOOS2: TYPE_CHECKING, NON_JOOS_PARAMETER_TYPE
// JAVAC:
public class Je_ConstructorWithNonJoosParameters 
{ 
	public Je_ConstructorWithNonJoosParameters() {
	} 
	
	public static int test() { 
		new Float(2);
		return 123;
	} 
}
