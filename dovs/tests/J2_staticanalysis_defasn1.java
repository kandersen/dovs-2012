// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT,CODE_GENERATION
public class J2_staticanalysis_defasn1 {

    public J2_staticanalysis_defasn1 () {}

    public static int test() {
	boolean i;
        boolean j = true;
	while (j || (i=false)) { 
	    j = false;
	}
	j = i;
        return 123;
    }

}
