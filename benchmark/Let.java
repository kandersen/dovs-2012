public class Let extends Expression {

    protected char _var;
    protected Expression _value;
    protected Expression _body;

    public Let(char var, Expression value, Expression body) {
	_var = var;
	_value = value;
	_body = body;
    }
    
    public int eval(Environment e) {
	int boundValue = _value.eval(e);
	return _body.eval(new Environment(_var, boundValue, e));
    }
}