// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_CLOSEST_MATCH_OVERLOADING
import java.io.Serializable;

public class J2_closestMatchMethod2 implements Serializable {
    public J2_closestMatchMethod2() {
    }

    public static int test() {
        return new J2_closestMatchMethod2().m("bla");
    }
    
    public int m(Object o) {
        return 123;
    }
}
