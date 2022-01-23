// HIERARCHY
/**
 * Hierarchy check:
 * - A class that has (declares or inherits) any abstract methods must
 * be abstract (8.1.1.1). (method equals(Object) inherited from superclass)
 */
import java.util.Comparator;

public class J1_4_AbstractMethod_InheritedFromInterface implements Comparator{

    public J1_4_AbstractMethod_InheritedFromInterface(){
    }

    public int compare(Object o1, Object o2){
	return o1.hashCode() - o2.hashCode() + 123;
    }

    public static int test(){
	J1_4_AbstractMethod_InheritedFromInterface object = new J1_4_AbstractMethod_InheritedFromInterface();
	return object.compare((Object)object, (Object)object);
    }

}
