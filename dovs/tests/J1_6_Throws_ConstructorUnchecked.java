// TYPE_CHECKING
public class J1_6_Throws_ConstructorUnchecked {

    public J1_6_Throws_ConstructorUnchecked() throws java.lang.RuntimeException { 
    }

    public static int test() {
        new J1_6_Throws_ConstructorUnchecked(); // allowed since java.lang.RuntimeException is unchecked
        return 123;
    }

}
