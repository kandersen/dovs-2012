/* TypeChecking:
 * 
 * Test for Protected Access
 * 
 * B.B extends A.A
 * C.C extends B.B
 * D.D extends C.C
 */

package C;

public class C extends B.B {
    public C() {}

    /* 
     * Looks like:
     * static method declared on subclass - access through this class
     * => FAIL, since D.D declares staticMethod() and C is not a subclass of D.D (6.6.2.1)
     */ 
    public void staticMethodAccessFromSubClass() {
    	// not a protected access - staticMethod is public
		D.D.staticMethod();
    }
}