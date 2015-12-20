package mikera.vectorz;

import clojure.lang.IFn.DDD;


/**
 * Wrapper class for a Clojure primitive function
 * @author Mike
 *
 */
public final class PrimitiveFnOp2 extends mikera.vectorz.Op2 {
	public PrimitiveFnOp2(Object f) {
		fn=(DDD)f;
	}
	
	public PrimitiveFnOp2(DDD f) {
		fn=(DDD)f;
	}
	
	protected final DDD fn;
	
	@Override
	public double apply(double x,double y) {
		return fn.invokePrim(x,y);
	}
	
	@Override 
	public void applyTo(double[] xs, int offset, int length, AVector ys) {
		for (int i=0; i<length; i++) {
			xs[offset+i]=fn.invokePrim(xs[offset+i],ys.unsafeGet(i));
		}
	}

}
