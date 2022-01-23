// PARSER_WEEDER
// JOOS1: JOOS1_OMITTED_CONSTRUCTOR
/**
 * If a class contains no explicit constructors, it implicitly
 * contains a public no-arg constructor which simply contains a no-arg
 * super statement.
 */
public class J2_1_OmittedConstructor{

    public int i = 123;

    public static int test(){
	J2_1_OmittedConstructor j = new J2_1_OmittedConstructor();
	return j.i;
    }

        public static void main(String[] args){
	System.out.println(J2_1_OmittedConstructor.test());
    }

}
