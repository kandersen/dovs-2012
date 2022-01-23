// DEFINITE_ASSIGNMENT,CODE_GENERATION
// JOOS1: JOOS1_OMITTED_LOCAL_INITIALIZER
public class J2_DefAss {

    public J2_DefAss () {}

    public static int test() {
        return new J2_DefAss().test(123);
    }


    public int test(int x) {
	int[] z;
	(z = new int[x])[z[x-1]] = x;
	return z[0];
    }

}
