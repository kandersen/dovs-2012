// JOOS1: DISAMBIGUATION,JOOS1_IMPLICIT_THIS_CLASS_STATIC_FIELD
// JOOS2: TYPE_CHECKING
import org.omg.CORBA.*;
public class J2_StaticFieldAccess implements ARG_IN {
    public J2_StaticFieldAccess(){}
    public static int test() {
	return value+122;
    }
}
