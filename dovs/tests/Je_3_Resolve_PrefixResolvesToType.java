// TYPE_LINKING
// JOOS1:TYPE_LINKING,PREFIX_RESOLVES_TO_TYPE
// JOOS2:TYPE_LINKING,PREFIX_RESOLVES_TO_TYPE
// JAVAC:
/* TypeLinking:
 * Check that no prefixes (consisting of whole identifiers) of fully qualified
 * types themselves resolve to types.
 * 
 * Should give the error that 'java.util.Map' resolves to a type, not that
 * 'java.util.Map.Entry' could not be found.
 */
public class Je_3_Resolve_PrefixResolvesToType {
    public Je_3_Resolve_PrefixResolvesToType() {}
    
    public static int test() {
	java.util.Map.Entry entry = null;
	return 123;
    }
}