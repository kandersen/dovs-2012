public class Constant extends Expression {

    protected int _value;

    public Constant(int value) {
	_value = value;
    }

    public int eval(Environment e) {
	return _value;
    }
}