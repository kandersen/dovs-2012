// TYPE_CHECKING
// JOOS1: NO_MATCHING_CONSTRUCTOR_FOUND,JOOS1_CLOSEST_MATCH_OVERLOADING
import java.io.Serializable;

public class J2_closestMatchConstructor2 implements Serializable {
    public int a;
        
    protected J2_closestMatchConstructor2(Object o) {
        a = 3;
    }
    
    protected J2_closestMatchConstructor2(Serializable o) {
        a = 123;
    }
    
    protected J2_closestMatchConstructor2(J2_closestMatchConstructor2 o) {
        a = 1;
    }

    public static int test() {
        return new J2_closestMatchConstructor2("").a;
    }
}
