// JOOS1:PARSER_WEEDER,NON_ABSTRACT_METHOD_BODY,PARSER_EXCEPTION
// JOOS2:PARSER_WEEDER,NON_ABSTRACT_METHOD_BODY,PARSER_EXCEPTION
// JAVAC:UNKNOWN
// 
/**
 * Parser/weeder:
 * - A non-abstract method must have a body.
 */
public class Je_1_Methods_NonAbstractNoBody {

    public Je_1_Methods_NonAbstractNoBody() { }

    public static int test() { 
	return 123; 
    }

    public void foo();

}
