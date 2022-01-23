// TYPE_CHECKING

import java.awt.image.DataBufferByte;

/**
 * Typecheck:
 * - Type byte[][] is assignable to type Object[]
 */
public class J1_6_Assignable_ByteMultArray_ObjectArray {

    public J1_6_Assignable_ByteMultArray_ObjectArray () {}

	public void method() {
    	DataBufferByte a = null;
        Object[] b = a.getBankData(); // getBankData() return byte[][]
	}

    public static int test() {
		return 123;
    }

}
