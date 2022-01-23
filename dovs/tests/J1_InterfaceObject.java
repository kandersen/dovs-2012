// HIERARCHY,TYPE_CHECKING
import java.util.*;

public class J1_InterfaceObject {
    public J1_InterfaceObject() {}

    public static int test() {
	Object x = (List)new LinkedList();
	return 123;
    }
}
