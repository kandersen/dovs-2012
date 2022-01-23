//JOOS1:PARSER_WEEDER,FORMAL_INITIALIZER,PARSER_EXCEPTION
//JOOS2:PARSER_WEEDER,FORMAL_INITIALIZER,PARSER_EXCEPTION
//JAVAC:UNKNOWN

public class Je_1_FormalParameterInitializer_Constructor {
	public int f;
	public Je_1_FormalParameterInitializer_Constructor(int i = 123) {
		f = i;
	}
	
	public static int test() {
		Je_1_FormalParameterInitializer_Constructor j = new Je_1_FormalParameterInitializer_Constructor();
		return j.f;
	}
}
		