// TYPE_LINKING
import java.util.List;
import java.util.LinkedList;
import java.awt.*;

public class J1_singleTypeImport {

    public J1_singleTypeImport () {}

    public static int test() {
	List ls = new LinkedList();
	java.awt.List als = null;
        return 123;
    }

}
