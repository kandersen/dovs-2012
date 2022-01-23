package dovs.testcase;
public class A {

    public int f = org.omg.CORBA.ARG_IN.value;

    public A () {}

    public int m() {
	return org.omg.CORBA.ARG_IN.value;
    }

    public static int m(int i) {
	return i+org.omg.CORBA.ARG_IN.value;
    }

    public static int test() {
	A m = new A();
	m.f = m.f+m.m()+dovs.testcase.A.m(m.f)+org.omg.CORBA.ARG_IN.value;
	java.lang.System.out.println("qualified");
	System.out.println("unqualified");
        return 118+m.f;
    }

}
