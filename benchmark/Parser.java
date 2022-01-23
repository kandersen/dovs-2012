public class Parser {

    protected String _input;

    public Parser(String input) {
	_input = input;
    }

    public Expression parseExpression() {
	if (_input.startsWith(" ")) {
	    _input = _input.substring(1);
	    return parseExpression();
	}
	
	if (_input.startsWith("(+")) {
	    eat("(+","");
	    Expression left = parseExpression();
	    Expression right = parseExpression();
	    eat(")", "Error, unterminated addition, expected: )");
	    return new Addition(left,right);
	}

	if (_input.startsWith("(*")) {
	    eat("(*","");
	    Expression left = parseExpression();
	    Expression right = parseExpression();
	    eat(")", "Error, unterminated multiplication, expected: )");
	    return new Multiplication(left,right);
	}

	if (_input.startsWith("(let")) {
	    eat("(let","");
	    char id = parseIdentifier();
	    Expression value = parseExpression();
	    Expression body = parseExpression();
	    eat(")", "Error, unterminated let, expected: )");
	    return new Let(id, value, body);
	}

	char firstChar = _input.charAt(0);
	if(isNumber(firstChar)) {
		String n = "";
		while (isNumber(firstChar)) {
		    n = n + firstChar;
		    _input = _input.substring(1);
		    if (_input.equals((Object) "")) {
			return new Constant(java.lang.Integer.parseInt(n));
		    }
		    firstChar = _input.charAt(0);
		}
		return new Constant(java.lang.Integer.parseInt(n));
	}

	return new Variable(parseIdentifier());
    }

    public boolean isNumber(char n) {
	return java.lang.Character.isDigit(n);
    }   

    public void eat(String s, String error) {
	if (_input.startsWith(" ")) {
	    _input = _input.substring(1);
	    eat(s, error);
	}

	if (_input.startsWith(s)) {
	    _input = _input.substring(s.length());
	} else {
	    System.out.println(error);
	    System.exit(1);
	}
    }
    
    public char parseIdentifier(){
	if (_input.equals((Object) "")) {
	    System.out.println("Error: Expected identifier!");
	    System.exit(1);
	    return ' ';
	}

	if (_input.startsWith(" ")) {
	    _input = _input.substring(1);
	    return parseIdentifier();
	}

	char firstCharacter = _input.charAt(0);
	if (isNumber(firstCharacter)) {
	    System.out.println("Error: illegal identifier!");
	    System.exit(1);
	    return ' ';
	}

	_input = _input.substring(1);
	return firstCharacter;
    }
}