// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT,CODE_GENERATION
public class J2_staticanalysis_defasn3 {

    public J2_staticanalysis_defasn3 () {}

    public static int test() {
	boolean i;
        boolean j = true;
	while ((i=false) && j) { 
	}
	j = i;
        return 123;
    }

}
