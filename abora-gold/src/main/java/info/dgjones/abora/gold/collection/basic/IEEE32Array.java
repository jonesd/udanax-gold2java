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

import java.io.PrintWriter;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tumbler.IEEE32Pos;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE32;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Concrete fixed size array that holds elements of the Java <code>float</code>
 * floating-point primitive data type. This is conceptually associated with
 * the single-precision 32-bit format IEEE 754 values.
 */
public class IEEE32Array extends PrimFloatArray {
	private final float[] storage;

	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(IEEE32Array.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	//////////////////////////////////////////////
	// Constructors

	/** 
	 * Construct a new array of the specified size with
	 * all elements initialized to zero.
	 *
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 * 
	 * @param count number of elements this will be able to hold
	 */
	protected IEEE32Array(int count) {
		super();
		storage = new float[count];
	}
	
	public IEEE32Array(Rcvr rcvr) {
		super(rcvr);
		int count = rcvr.receiveUInt32();
		storage = new float[count];
		for (int i = 0; i < storage.length; i++) {
			storeFloat(i, rcvr.receiveIEEEDoubleVar());
		}
	}


	/** 
	 * Construct a new array of the specified size with
	 * all elements initialized to zero.
	 *
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 * 
	 * @param count number of elements this will be able to hold
	 */

	protected IEEE32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	/** 
	 * Construct a new array of the same size as the specified source
	 * and containing a copy of its content. 
	 *
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 * 
	 * @param source primitive array to copy
	 */
	protected IEEE32Array(float[] source) {
		this(source.length);
		System.arraycopy(source, 0, storage, 0, source.length);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimFloatArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** 
	 * Return a new IEEE32Array of the specified size suitable for
	 * holding IEEE32 values, initially filled with zeros.
	 *  
	 * @param count number of elements this will be able to hold
	 */
	public static IEEE32Array make(int count) {
		return new IEEE32Array(count);
	}

	/** create an IEEE32Array filled with the indicated data in 'from' */
	public static IEEE32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new IEEE32Array(size, from, sourceOffset, count, destOffset);
	}

	public static IEEE32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IEEE32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IEEE32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static IEEE32Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an IEEE32Array filled with the data at 'buffer' */
	public static IEEE32Array make(float[] buffer) {
		return new IEEE32Array(buffer);
	}

	//////////////////////////////////////////////
	// accessing

	/**
	 * Store a floating point <code>value</code> at the specified <code>index</code>.
	 * 
	 * @param index index in array the element will be stored at.
	 * @param value floating point value to store in <code>this</code>.
	 */
	public void storeIEEE32(int index, float value) {
		storage[index] = value;
	}

	/**
	 * Return the floating point number at the specified <code>index</code>.
	 * 
	 * @param index 0 based index of <code>this</code> to read the number from.
	 * @return the read floating point number
	 */
	public float iEEE32At(int index) {
		return storage[index];
	}

	public void storeFloat(int index, double value) {
		storeIEEE32(index, (float) value);
	}

	public double floatAt(int index) {
		return (double) iEEE32At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeIEEE32(index, ((PrimFloatValue) value).asIEEE32());
	}

	public Heaper fetchValue(int index) {
		return PrimIEEE32.make(iEEE32At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		return PrimSpec.iEEE32();
	}

	/** Return the maximum word size that can be stored in this array */
	public int bitCount() {
		return 32;
	}

	//////////////////////////////////////////////
	// Bulk Storing

	public void storeAll(Heaper value, int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		float floatValue;
		if (value == null) {
			floatValue = 0.0f;
		} else {
			floatValue = ((PrimFloatValue) value).asIEEE32();
		}
		for (int i = 0; i < n; i += 1) {
			storeIEEE32(start + i, floatValue);
		}
	}

	/** 
	 * Copy a consequitive range of elements from the receiver into the
	 * supplied buffer.
	 *  
	 * @param buffer array to fill with receveirs elements
	 * @param count number of consequentive elements in range or all
	 * 			elements from start if -1. Silently truncate if count is
	 * 			larger than available elements in the receiver
	 * @param start index of first element in range
	 */
	public void copyToBuffer(float[] buffer, int count, int start) {
		int n;
		if (count >= 0) {
			n = count;
		} else {
			n = count() - start;
		}
		if (n > buffer.length) {
			n = buffer.length;
		}
		System.arraycopy(storage, start, buffer, 0, n);
	}

	//////////////////////////////////////////////
	// Comparing and Hashing

	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				float cmp = iEEE32At(i + myStart) - o.iEEE32At(i + otherStart);
				if (cmp != 0.0) {
					return cmp < 0.0f ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(myStart, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int start) {
		for (int i = start; i < count(); i += 1) {
			float val = iEEE32At(i);
			if (val < 0.0) {
				return -1;
			}
			if (val > 0.0) {
				return +1;
			}
		}
		return 0;
	}

	//////////////////////////////////////////////
	// Arithmetic Manipulations

	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE32(i + myStart, iEEE32At(i + myStart) + o.iEEE32At(i + otherStart));
			}
		} else {
			super.addData(myStart, other, otherStart, count);
		}
	}

	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE32(i + myStart, iEEE32At(i + myStart) - o.iEEE32At(i + otherStart));
			}
		} else {
			super.subtractData(myStart, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(iEEE32At(index));
	}
	
	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendUInt32(count());
		for (int i = 0; i < storage.length; i++) {
			float f = storage[i];
			xmtr.sendIEEEDoubleVar(f);
		}
	}

}
