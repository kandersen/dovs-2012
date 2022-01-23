import java.io.*;

public class Interpreter {

    public Interpreter(){
    }

    public static void main(String[] args) throws IOException {
	if (args.length == 0) {

	    while (true) {
		System.out.print("> ");
		String input = new BufferedReader((Reader) new InputStreamReader(System.in)).readLine();

		if (input.equals((Object) "exit")) {		  
		    return;
		}

		Parser parser = new Parser(input);
		Expression aexp = parser.parseExpression();
		System.out.println(aexp.eval(new Environment()));
	    }	    

	} else {
	    Parser parser = new Parser(args[0]);
	    Expression aexp = parser.parseExpression();
	    System.out.println(aexp.eval(new Environment()));	    
	    return;
	}
    }
}