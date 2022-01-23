// DEFINITE_ASSIGNMENT,CODE_GENERATION
// JOOS1: JOOS1_LOCAL_VARIABLE_IN_OWN_INITIALIZER
public class J2_defasn_init {
    public J2_defasn_init() {}
    public static int test() {
	int a = (a=41)+a+a;
	return a;
    }
}
