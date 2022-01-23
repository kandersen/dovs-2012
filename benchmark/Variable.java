public class Variable extends Expression {

    protected char _var;

    public Variable(char var) {
	_var = var;
    }

    public int eval(Environment e) {
	return e.lookup(_var);
    }
}