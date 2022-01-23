// CODE_GENERATION
public class J1_divisionbyzero {

    public J1_divisionbyzero () {}

    public static int test() throws ArithmeticException {
	int i = 0;
	System.out.println(i/i);
        return 123;
    }

}
