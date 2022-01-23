public class Environment {
    
    protected Environment _rest;
    protected char _var;
    protected int _value;

    public Environment() {
	_rest = null;
	_var = ' ';
	_value = 0;
    }

    public Environment(char var, int value, Environment rest) {
	_rest = rest;
	_var = var;
	_value = value;
    }
    
    public int lookup(char var) {
	if (_var == var) {
	    return _value;
	}
	
	if (_rest == null) {
	    System.out.println("Error: Unbound variable " + var + "!");
	    System.exit(1);
	    return 0;
	}

	return _rest.lookup(var);
    }    
}

    