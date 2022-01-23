//JOOS1:PARSER_WEEDER,PARSER_EXCEPTION
//JOOS2:PARSER_WEEDER,PARSER_EXCEPTION
//JAVAC:UNKNOWN 

public class Je_1_FormalCommaPrefix {
	public Je_1_FormalCommaPrefix() {}

	public void foo(,int i) {}
	
	public static int test() {
		return 123;
	}
}
