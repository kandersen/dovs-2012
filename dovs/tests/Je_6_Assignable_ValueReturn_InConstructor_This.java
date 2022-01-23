//JOOS1:TYPE_CHECKING,ASSIGN_TYPE
//JOOS2:TYPE_CHECKING,ASSIGN_TYPE
//JAVAC:UNKNOWN

public class Je_6_Assignable_ValueReturn_InConstructor_This {	
	public static int test() {
		return 123;
	}
	
	public Je_6_Assignable_ValueReturn_InConstructor_This() {
		return this;
	}
}
