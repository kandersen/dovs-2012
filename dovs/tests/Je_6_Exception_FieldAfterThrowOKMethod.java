// JOOS1:TYPE_CHECKING,ILLEGAL_THROWS
// JOOS2:TYPE_CHECKING,ILLEGAL_THROWS
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - Tests whether throwok check works for fields declared after some methods.
 */

public class Je_6_Exception_FieldAfterThrowOKMethod{

    public Je_6_Exception_FieldAfterThrowOKMethod(){
    }

    public Je_6_Exception_FieldAfterThrowOKMethod(int i) throws java.io.IOException{}

    public void foo() throws java.io.IOException{
	
    }

    public Je_6_Exception_FieldAfterThrowOKMethod a = new Je_6_Exception_FieldAfterThrowOKMethod(123);

    public static int test(){
	return 123;
    }
}
