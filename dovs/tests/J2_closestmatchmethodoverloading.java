// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_CLOSEST_MATCH_OVERLOADING
public class J2_closestmatchmethodoverloading {

    public J2_closestmatchmethodoverloading() {}
    
    public int m1(Object x, Object y) {
	return 42;
    }

    public int m1(Object x, J2_closestmatchmethodoverloading y) {
	return 123;
    }

    public int m2() {
	return this.m1(new J2_closestmatchmethodoverloading(), 
		       new J2_closestmatchmethodoverloading());
    }

    public static int test() {
	return new J2_closestmatchmethodoverloading().m2();
    }

}
