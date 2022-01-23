// CODE_GENERATION

/** CodeGeneration:
 *  This testcase tests the implementation of (eager) xor.
 */
public class J1_A_EagerXor_Sideeffect {
	public J1_A_EagerXor_Sideeffect() {}
	
	public int value = 1;
	
	public boolean m1() {
		value = value + 1;
		return false;
	}
	
	public boolean m2() {
		value = value * 2;
		return false;
	}
  
	public static int test() {
		// below should evaluate as
		// j.value = 1;
		// j.value = j.value + 1 => j.value = 2
		// j.value = j.value * 2 => j.value = 4
		J1_A_EagerXor_Sideeffect j = new J1_A_EagerXor_Sideeffect();
		if (j.m1() ^ j.m2()) {
			return 42;
		}
		return 119 + j.value;
	}
}
