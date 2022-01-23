// JOOS1: DEFINITE_ASSIGNMENT,JOOS1_OMITTED_LOCAL_INITIALIZER
// JOOS2: DEFINITE_ASSIGNMENT,CODE_GENERATION
public class J2_staticanalysis_defasn2 {
	
	public J2_staticanalysis_defasn2 () {}
	
	public static int test() {
		return 123;
	}

	public void m() {
		boolean i;
		boolean j = true;
		if (j || (i=false)) { 
			i = j;
		}
	}
}
