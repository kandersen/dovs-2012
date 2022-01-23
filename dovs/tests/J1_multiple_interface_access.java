// DISAMBIGUATION,HIERARCHY
/* When multiple interfaces are implemented, do we have access to them all? */

import org.omg.CORBA.ARG_IN;
import java.io.ObjectStreamConstants;

public class J1_multiple_interface_access implements ObjectStreamConstants, ARG_IN {
    public J1_multiple_interface_access() {}
    public static int test() {
	int v = J1_multiple_interface_access.value;
	int p = J1_multiple_interface_access.PROTOCOL_VERSION_1;
	return 123;
    }
}
