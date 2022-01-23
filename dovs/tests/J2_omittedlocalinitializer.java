// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT,CODE_GENERATION
public class J2_omittedlocalinitializer {
    public J2_omittedlocalinitializer() {}
    public int m() {
	int x;
	int y;
	y = 0;
	x = y+123;
	return x;
    }
    public static int test() {
	return new J2_omittedlocalinitializer().m();
    }
}

