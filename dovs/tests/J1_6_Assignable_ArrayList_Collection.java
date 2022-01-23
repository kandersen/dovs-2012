// TYPE_CHECKING
// 
/**
 * Typecheck:
 * - Type ArrayList is assignable to type Collection
 */
import java.util.*;
 
public class J1_6_Assignable_ArrayList_Collection {

    public J1_6_Assignable_ArrayList_Collection () {}

    public static int test() {
        Collection o = new ArrayList();
		return 123;
    }

}
