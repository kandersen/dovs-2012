// JOOS1:TYPE_CHECKING,FIELD_ON_ARRAY
// JOOS2:TYPE_CHECKING,FIELD_ON_ARRAY
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - On .length can be used on arrays
 */
public class Je_6_Array_Field{

    public Je_6_Array_Field(){}

    public static int test(){
		int[] a = new int[42];
		int b = a.field;
		return 123;
    }

}
