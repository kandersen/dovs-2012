// DEFINITE_ASSIGNMENT
// JOOS1: JOOS1_OMITTED_LOCAL_INITIALIZER
/**
 * Parser/weeder:
 * - (Joos 1) Omitted local initializer
 * - (Joos 2) Tests whether local variable declarations in for init
 * are allowed to have no initalizer
 */
public class J2_1_For_InitDeclWithoutInitializer{
    
    public J2_1_For_InitDeclWithoutInitializer(){}

    public static int test(){
	int i = 0;
	for (int j; i <= 500; i = i + 1){
	    j = i;
	    if (i == 123){
		return j;
	    }
	}
	return i;
    }

}
