// JOOS1: TYPE_CHECKING, ASSIGN_TYPE
// JOOS2: TYPE_CHECKING, ASSIGN_TYPE
// JAVAC: UNKNOWN
public class Je_AssignPrimitiveArrayToObjectArray {
	
	public Je_AssignPrimitiveArrayToObjectArray() {
	}
	
	public static int test() {
		Object[] array = new int[123];
		return array.length;
	}
	
}