// DEFINITE_ASSIGNMENT,CODE_GENERATION
// JOOS1: JOOS1_LOCAL_VARIABLE_IN_OWN_INITIALIZER
public class J2_defasn_init2 {
    public J2_defasn_init2() {}
    public static int test() {
	int a = (a=123);
	return a;
    }
}
