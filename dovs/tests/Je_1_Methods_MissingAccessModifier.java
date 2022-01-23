// JOOS1:PARSER_WEEDER,PARSER_EXCEPTION,SYNTAX_ERROR
// JOOS2:PARSER_WEEDER,PARSER_EXCEPTION,SYNTAX_ERROR
// JAVAC:
/**
 * Parser/weeder:
 * - Methods must have a modifier
 */
public class Je_1_Methods_MissingAccessModifier {

    public Je_1_Methods_MissingAccessModifier() { }
    
    public static int test() { 
        return 123;
    }
    
    int bar() {
        return 123;
    }             
}
