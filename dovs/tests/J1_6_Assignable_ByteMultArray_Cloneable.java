// TYPE_CHECKING

import java.awt.image.DataBufferByte;

/**
 * Typecheck:
 * - Type byte[][] is assignable to type Object[]
 */
public class J1_6_Assignable_ByteMultArray_Cloneable {

    public J1_6_Assignable_ByteMultArray_Cloneable () {}

	public void method() {
    	DataBufferByte a = null;
        Cloneable b = a.getBankData(); // getBankData() return byte[][]
	}

    public static int test() {
		return 123;
    }

}
