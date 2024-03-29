/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.collection.basic;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE64;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Fixed size array containing floating point numbers of the same precision.
 * <p>
 * The <code>double</code> primitive type is assumed to have the greatest
 * precision stored by all subclass implementations and so is used to implement
 * general floating-point implementations. Subclasses should reimplement operations for
 * performance or where their specific element type has to be reflected in the
 * API.
 * <p>
 * You should be aware that the methods defined at this level will often convert
 * array element values into <code>double</code> primitive type before operating
 * on the value. Sometimes when converting from float to double values values can
 * subtely change. This may manifest in operations such as comparison producing
 * unexpected results.
 */
public abstract class PrimFloatArray extends PrimDataArray {


	//////////////////////////////////////////////
	// Constructors

	/**
	 * Construct a new array.
	 * <p>
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 */
	protected PrimFloatArray() {
		super();
	}
	
	public PrimFloatArray(Rcvr rcvr) {
		super(rcvr);
	}


//	/** Make an array initialized to zero values */
//	public static PrimFloatArray zeros(int bitCount, int count) {
//		throw new UnsupportedOperationException();
//	}
//

	//////////////////////////////////////////////
	// Accessing
	
	/** 
	 * Store a floating point number at the specified <code>index</code>.
	 * 
	 * @param index 0 based index to store the <code>value</code> at.
	 * @param value value to store in array, may lose precision if
	 * 	array cant hold the full extent of it. 
	 */
	public abstract void storeFloat(int index, double value);

	/**
	 * Return the floating point number at the specified <code>index</code>.
	 * 
	 * @param index 0 based index of this to read the number from.
	 * @return value at the specified index. 
	 */
	public abstract double floatAt(int index);

	/**
	 * Return the maximum bits/entry that can be stored in this array.
	 */
	public abstract int bitCount();


	//////////////////////////////////////////////
	// Comparing and Hashing
	
	public int elementsHash(int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		if (n == 0) {
			return FHash.fastHash(17);
		} else {
			if (n == 1) {
				return FHash.fastHash(floatAt(start));
			} else {
				return FHash.fastHash(n) ^ FHash.fastHash(floatAt(start)) ^ FHash.fastHash(floatAt(start + n - 1));
			}
		}
	}

	protected int compareData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				double cmp = floatAt(i + start) - o.floatAt(i + otherStart);
				if (cmp != 0.0) {
					return cmp < 0.0 ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}


	//////////////////////////////////////////////
	// Searching/Finding

	public int indexOf(Heaper value, int start, int nth) {
		//TODO contents of indexOf && indexPast are the same except
		// whether the value should, or shoud not, match elements
		// Refactor out private shared method with exclusive-or check
		
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		double x = ((PrimFloatValue) value).asIEEE64();

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				if (floatAt(idx) == x) {
					nth -= 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				if (floatAt(idx) == x) {
					nth += 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
	}

	public int indexPast(Heaper value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		double x = ((PrimFloatValue) value).asIEEE64();

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				if (floatAt(idx) != x) {
					nth -= 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				if (floatAt(idx) != x) {
					nth += 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
	}


	//////////////////////////////////////////////
	// Bulk Storage
	
	protected Heaper zeroElement() {
		//TODO how many of these are we creating?
		return PrimIEEE64.make(0.0);
	}

	//////////////////////////////////////////////
	// Arithmetic Manipulations

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				storeFloat(i + start, floatAt(i + start) + o.floatAt(i + otherStart));
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				storeFloat(i + start, floatAt(i + start) - o.floatAt(i + otherStart));
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	public static PrimFloatArray zeros(int i, int j) {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(PrimArray array, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}
}
