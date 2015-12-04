package mikera.vectorz;

import clojure.lang.IFn;
import clojure.lang.IFn.DD;


/**
 * Wrapper class for a Clojure arity-1 function
 * @author Mike
 *
 */
public final class FnOp extends mikera.vectorz.ops.AFunctionOp {
	protected IFn fn;
	
	public static Op wrap(Object f) {
		if (f instanceof DD) {
			return new PrimitiveFnOp((DD)f);
		} else {
			return new FnOp(f);
		}
	}

	public FnOp(Object f) {
		fn=(IFn)f;
	}
	
	public FnOp(IFn f) {
		fn=f;
	}
	
	@Override
	public double apply(double x) {
		return ((Number)fn.invoke(x)).doubleValue();
	}
	
	@Override 
	public void applyTo(double[] xs, int offset, int length) {
		for (int i=0; i<length; i++) {
			xs[i]=((Number)fn.invoke(xs[i])).doubleValue();
		}
	}

}
