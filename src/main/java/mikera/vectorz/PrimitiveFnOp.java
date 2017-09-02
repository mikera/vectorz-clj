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
			int ii=offset+i;
			xs[ii]=fn.invokePrim(xs[ii]);
		}
	}
	
	@Override
	public void applyTo(double[] data, int offset, int stride, int length) {
		if (stride==1) {
			applyTo(data,offset,length);
		} else {
			for (int i=0; i<length; i++) {
				int ii=offset+i*stride;
				double x=data[ii];
				data[ii]=fn.invokePrim(x);
			}
		}
	}

}
