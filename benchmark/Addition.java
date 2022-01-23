public class Addition extends Expression {

    protected Expression _left;
    protected Expression _right;
    
    public Addition(Expression left, Expression right) {
	_left = left;
	_right = right;
    }

    public int eval(Environment e) {
	int l = _left.eval(e);
	int r = _right.eval(e);
	return l + r;
    }
}