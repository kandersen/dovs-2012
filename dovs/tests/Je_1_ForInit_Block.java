//JOOS1:PARSER_WEEDER,PARSER_EXCEPTION
//JOOS2:PARSER_WEEDER,PARSER_EXCEPTION
//JAVAC:UNKNOWN

public class Je_1_ForInit_Block {
	public Je_1_ForInit_Block() {
		int i = 0;
		for ({int j = 0;} i < 10 ; i = i + 1) {
		}
	}
	
	public static int test() {
		return 123;
	}
}