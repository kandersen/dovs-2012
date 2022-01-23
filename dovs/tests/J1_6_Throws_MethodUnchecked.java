// TYPE_CHECKING
public class J1_6_Throws_MethodUnchecked {
	public J1_6_Throws_MethodUnchecked() {}

    public void m() throws java.lang.RuntimeException { 
    }

    public static int test() {
        J1_6_Throws_MethodUnchecked o = new J1_6_Throws_MethodUnchecked();
        o.m(); // allowed since java.lang.RuntimeException is unchecked
        return 123;
    }

}
