// CODE_GENERATION
public class J1_FieldInitOrder {
	
	public J1_FieldInitOrder() {}
	
	public int[] array = new int[20];
	public int index;
	public int h0 = append(0);
	public int u1 = append(1);
	public int e2 = append(2);
	public int f3 = append(3);
	public int r4 = append(4);
	public int f5 = append(5);
	public int g6 = append(6);
	public int q7 = append(7);
	public int d8 = append(8);
	public int x9 = append(9);
	public int x10 = append(10);
	public int z11 = append(11);
	public int f12 = append(12);
	public int a13 = append(13);
	public int z14 = append(14);
	public int y15 = append(15);
	public int f16 = append(16);
	public int j17 = append(17);
	public int d18 = append(18);
	public int i19 = append(19);
	
	public int append(int x) {
		this.array[this.index] = x;
		this.index = this.index + 1;
		return 0;
	}
	
	public static int test() {
		int[] array = new J1_FieldInitOrder().array;
		for (int i=0; i<20; i=i+1) {
			if (array[i] != i)
				return 0;
		}
		return 123;
	}
}