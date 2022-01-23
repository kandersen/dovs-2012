// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: DISAMBIGUATION,TYPE_CHECKING
public class J2_nonstatic_on_static_from_static {
	public static String mystring = "a123";
	public J2_nonstatic_on_static_from_static() { }
	public static int test() {
		return Integer.parseInt(mystring.substring(1));
	}
}
