// JOOS1: PARSER_WEEDER,JOOS1_STATIC_FIELD_DECLARATION,PARSER_EXCEPTION
// JOOS2: DISAMBIGUATION
public class J2_static_shared {
    public J2_static_shared() {}
    

    protected static int shared = 0;
    protected int mine = 0;

    
    public static int test() {
	J2_static_shared o1 = new J2_static_shared();
	J2_static_shared o2 = new J2_static_shared();
	
	o1.bump( 3, 1000);
	o2.bump(20, 1001);
	bump(100);
	return shared * (o2.mine - o1.mine);
    }

    public static void bump(int ds) {
	shared = shared + ds;
    }

    public void bump(int ds, int dm) {
	shared = shared + ds;
	mine   = mine   + dm;
    }
}
