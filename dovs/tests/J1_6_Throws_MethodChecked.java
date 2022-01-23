// TYPE_CHECKING
public class J1_6_Throws_MethodChecked {

    public J1_6_Throws_MethodChecked() {}
    
    public void m() throws java.io.FileNotFoundException, java.sql.SQLException {
    	new java.io.FileReader("Non-existing file ==> FileNotFoundException!");
    }

    public static int test() throws java.io.IOException, java.sql.SQLException {
		J1_6_Throws_MethodChecked o = new J1_6_Throws_MethodChecked(); // allowed since FileNotFoundException <: IOException and SQLException <: SQLException
		o.m();
        return 123;
    }

}
