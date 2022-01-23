// CODE_GENERATION
public class J1_stringadd {
    public J1_stringadd() {}

    public static int test() {
	String s = 2+4+""+4+2;
	return Integer.parseInt(new StringBuffer().append(s).reverse().toString())/2;
    }
}
