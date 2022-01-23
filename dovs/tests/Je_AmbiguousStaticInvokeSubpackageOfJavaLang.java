// JOOS1: DISAMBIGUATION, VARIABLE_NOT_FOUND, VARIABLE_OR_TYPE_NOT_FOUND
// JOOS2: DISAMBIGUATION, VARIABLE_NOT_FOUND, VARIABLE_OR_TYPE_NOT_FOUND
// JAVAC: UNKNOWN
public class Je_AmbiguousStaticInvokeSubpackageOfJavaLang {
	public Je_AmbiguousStaticInvokeSubpackageOfJavaLang() {}
	
	public static int test() {
		// there exists a static method:
		//    java.lang.reflect.AccessibleObject.setAccessible
		// but this is not the method we refer to here
		reflect.AccessibleObject.setAccessible();
		return 123;
	}
}