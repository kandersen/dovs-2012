// JOOS1:TYPE_CHECKING,INSTANTIATE_ABSTRACT_CLASS
// JOOS2:TYPE_CHECKING,INSTANTIATE_ABSTRACT_CLASS
// JAVAC:UNKNOWN
// 
/**
 * Typecheck:
 * - The type in a class instance creation expression must be a
 * non-abstract class.
 */
public abstract class Je_6_InstantiateAbstract_FromAbstract {

    public Je_6_InstantiateAbstract_FromAbstract () {}

    public static int test() {
	Number j = new Number();
	return 123;
    }

}
