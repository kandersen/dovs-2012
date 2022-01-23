//JOOS1:PARSER_WEEDER,FORMAL_INITIALIZER,PARSER_EXCEPTION
//JOOS2:PARSER_WEEDER,FORMAL_INITIALIZER,PARSER_EXCEPTION
//JAVAC:UNKNOWN

public class Je_1_FormalParameterInitializer_Method {
	public Je_1_FormalParameterInitializer_Method() {
	}
	
	public int m(int i = 123) {
		return i;
	}
	
	public static int test() {
		Je_1_FormalParameterInitializer_Method j = new Je_1_FormalParameterInitializer_Method();
		return j.m();
	}
}
		