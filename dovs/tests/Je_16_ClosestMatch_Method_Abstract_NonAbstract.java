// TYPE_CHECKING
// JOOS1: NO_MATCHING_METHOD_FOUND,JOOS1_CLOSEST_MATCH_OVERLOADING,AMBIGUOUS_OVERLOADING
// JOOS2: AMBIGUOUS_OVERLOADING
// JAVAC: UNKNOWN

public abstract class Je_16_ClosestMatch_Method_Abstract_NonAbstract {
    public Je_16_ClosestMatch_Method_Abstract_NonAbstract() {}

    public static int test() {
    	return 123;
    }
    
    public void foo(Je_16_ClosestMatch_Method_Abstract_NonAbstract j) {
        j.m("", ""); // this call is ambiguous
    }
    
    public void m(Object o, String s) {}
    
    public abstract void m(String o, Object s);
}