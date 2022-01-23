// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: DISAMBIGUATION,ILLEGAL_FORWARD_STATIC_FIELD_REFERENCE
// JAVAC:UNKNOWN
/**
 * Parser/weeder:
 * - (Joos 1) No static field declarations allowed
 * Disambiguation:
 * - (Joos 2) The initializer for a static field must not refer by
 * simple name to itself or a static field declared later in the same
 * class, except as the direct left-hand side of an assignment.
 */
public class Je_15_ForwardReference_StaticFields {

    public static int field1 = 23 + field2;

    public static int field2 = 100;

    public Je_15_ForwardReference_StaticFields() { }

    public static int test() { 
	return field1; 
    }

}
