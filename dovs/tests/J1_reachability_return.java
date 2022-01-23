// REACHABILITY
import java.util.Random;

public class J1_reachability_return {

    public Random random = new Random();

    public J1_reachability_return () {}

    public void m() {
	if (random.nextBoolean()) {
	    System.out.println("then branch completes normally!");
	}
	else {
	    return;
	}
    }

    public void m1() {
	if (random.nextBoolean()) {
	    return;
	}
	else {
	    return;
	}
    }

    public static int test() {
	J1_reachability_return j = new J1_reachability_return();
        j.m();
	j.m1();
        return 123;
    }

}
