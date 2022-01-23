// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT,CODE_GENERATION
public class J2_staticanalysis_defasn {

    public J2_staticanalysis_defasn () {}

    public static int test() {
	int i;
	int j = 0;
	while (j!=(i=0)) { }
	j = i;
        return 123;
    }

}
