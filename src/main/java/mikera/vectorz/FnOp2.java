package mikera.vectorz;

import clojure.lang.IFn;
import clojure.lang.IFn.DDD;


/**
 * Wrapper class for a Clojure arity-1 function
 * @author Mike
 *
 */
public final class FnOp2 extends mikera.vectorz.Op2 {
	protected IFn fn;
	
	public static Op2 wrap(Object f) {
		if (f instanceof DDD) {
			return new PrimitiveFnOp2((DDD)f);
		} else {
			return new FnOp2(f);
		}
	}

	public FnOp2(Object f) {
		fn=(IFn)f;
	}
	
	public FnOp2(IFn f) {
		fn=f;
	}
	
	@Override
	public double apply(double x, double y) {
		return ((Number)fn.invoke(x,y)).doubleValue();
	}
	
	@Override 
	public void applyTo(double[] xs, int offset, int length, AVector ys) {
		for (int i=0; i<length; i++) {
			xs[i]=((Number)fn.invoke(xs[i],ys.unsafeGet(i))).doubleValue();
		}
	}
}
