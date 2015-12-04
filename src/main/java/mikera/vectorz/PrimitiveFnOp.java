package mikera.vectorz;

import clojure.lang.IFn.DD;


/**
 * Wrapper class for a Clojure primitive function
 * @author Mike
 *
 */
public final class PrimitiveFnOp extends mikera.vectorz.ops.AFunctionOp {
	public PrimitiveFnOp(Object f) {
		fn=(DD)f;
	}
	
	public PrimitiveFnOp(DD f) {
		fn=(DD)f;
	}
	
	protected clojure.lang.IFn.DD fn;
	
	@Override
	public double apply(double x) {
		return fn.invokePrim(x);
	}
	
	@Override 
	public void applyTo(double[] xs, int offset, int length) {
		for (int i=0; i<length; i++) {
			xs[i]=fn.invokePrim(xs[i]);
		}
	}

}
