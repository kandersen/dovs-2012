// TYPE_CHECKING
public class J1_typecheck_constructor_invocation {

    public J1_typecheck_constructor_invocation (int i, String s) throws java.io.FileNotFoundException, java.sql.SQLException { 
	new java.io.FileReader("Non-existing file ==> FileNotFoundException!");
    }

    public static int test() throws java.io.IOException, java.sql.SQLException {
	new J1_typecheck_constructor_invocation(7, "flimflam");
        return 123;
    }

}
