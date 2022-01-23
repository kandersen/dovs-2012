// TYPE_CHECKING
// JOOS1: JOOS1_BITWISE_OPERATIONS
public class J2_bitwiseoperations {
    public J2_bitwiseoperations() {}
    public static int test() {
	int x = 123;
	return (int)((x & x) | x) ^ 0;
    }
}
