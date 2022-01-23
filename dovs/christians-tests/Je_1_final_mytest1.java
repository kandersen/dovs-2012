// PARSER_WEEDER
// JOOS1:PARSER_EXCEPTION,JOOS1_FINAL_FIELD_DECLARATION,JOOS1_STATIC_FIELD_DECLARATION
// JOOS2:PARSER_EXCEPTION,MISSING_FINAL_FIELD_INITIALIZER
// JAVAC:UNKNOWN
/**
 * Parser/weeder:
 * - (Joos 1) No final field declarations allowed,
 * - (Joos 2) A final field must have an initializer.
 */
public class Je_1_final_mytest1 {

    public final static int result;

    public Je_1_final_mytest1(){
    }
    
    public static int test(){
      return 123;
    }

}
