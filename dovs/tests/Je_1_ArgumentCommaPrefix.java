//JOOS1:PARSER_WEEDER,PARSER_EXCEPTION
//JOOS2:PARSER_WEEDER,PARSER_EXCEPTION
//JAVAC:UNKNOWN

public class Je_1_ArgumentCommaPrefix {
	public Je_1_ArgumentCommaPrefix() {}

	public void foo(int i) {
		foo(,0);
	}
	
	public static int test() {
		return 123;
	}
}
