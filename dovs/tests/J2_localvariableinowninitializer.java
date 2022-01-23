// DEFINITE_ASSIGNMENT,CODE_GENERATION
// JOOS1: JOOS1_LOCAL_VARIABLE_IN_OWN_INITIALIZER
public class J2_localvariableinowninitializer {
    public J2_localvariableinowninitializer() {}
    public static int test() {
	int x = 121 + (x = 1) + x;
	return x;	
    }
}

