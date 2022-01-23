// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT,CODE_GENERATION
public class J2_staticanalysis_defasn4 {

    public J2_staticanalysis_defasn4 () {}

    public static int test() {
        return 123;
    }

    public void m() {
	boolean i;
        boolean j = false;
	while (true || (j || (i=true))) { 
	    j = false;
	}
	j = i;
    }
}
