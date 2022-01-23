// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_CLOSEST_MATCH_OVERLOADING
import java.io.Serializable;

public class J2_closestMatchMethod1 implements Serializable {
    public J2_closestMatchMethod1() {
    }

    public static int test() {
        return new J2_closestMatchMethod1().m(new J2_closestMatchMethod1());
    }
    
    public int m() {
        return 1;
    }
        
    public int m(java.util.List o) {
        return 16;
    }
    
    public int m(Serializable o) {
        return 123;
    }
        
    public int m(Object o, int a) {
        return 4;
    }
    
    public int m(Object o) {
        return 14;
    }
}
