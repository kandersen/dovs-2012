//PARSER_WEEDER
//JOOS1:PARSER_EXCEPTION,SYNTAX_ERROR,JOOS1_MULTI_ARRAY
//JOOS2:PARSER_EXCEPTION,SYNTAX_ERROR
//JAVAC:UNKNOWN

public class Je_1_MultiArrayCreation_MissingDimension_4 {
	
	public Je_1_MultiArrayCreation_MissingDimension_4() {}

	public static int test() {
		int[][][] array = new int[2][][2];
		return 123;
	}
	
}
