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
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimIntegerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Int32Array extends PrimIntArray {
	private final int[] storage;

	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(Int32Array.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	//////////////////////////////////////////////
	// Constructors

	protected Int32Array(int count) {
		super();
		storage = new int[count];
	}
	
	public Int32Array(Rcvr rcvr) {
		super(rcvr);
		int count = rcvr.receiveUInt32();
		storage = new int[count];
		for (int i = 0; i < storage.length; i++) {
			storeInteger(i, rcvr.receiveInt32());
		}
	}


	protected Int32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected Int32Array(int[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an Int32Array filled with zeros */
	public static Int32Array make(int count) {
		return new Int32Array(count);
	}

	/** create an Int32Array filled with the indicated data in 'from' */
	public static Int32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new Int32Array(size, from, sourceOffset, count, destOffset);
	}

	public static Int32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static Int32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static Int32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static Int32Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an Int32Array filled with the data at 'buffer' */
	public static Int32Array make(int[] buffer) {
		return new Int32Array(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store a 32 bit signed integer value */
	public void storeInt32(int index, int value) {
		storage[index] = value;
	}

	/** Get a 32 bit signed actual integer value */
	public int int32At(int index) {
		return storage[index];
	}

	public void storeInteger(int index, int value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeInt32(index, value);
	}

	public int integerAt(int index) {
		return int32At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		PrimIntValue integerValue = (PrimIntValue)value;
		storeInteger(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		return PrimIntValue.make(int32At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		return PrimSpec.int32();
	}

	public int bitCount() {
		return -32;
	}

	//////////////////////////////////////////////
	// Bulk Storage

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
	public void copyToBuffer(int[] buffer, int count, int start) {
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

	protected int compareData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof Int32Array) {
			Int32Array o = (Int32Array) other;
			for (int i = 0; i < count; i += 1) {
				int cmp1 = int32At(i + start);
				int cmp2 = o.int32At(i + otherStart);
				if (cmp1 < cmp2) {
					return -1;
				} else if (cmp1 > cmp2) {
					return +1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int index) {
		for (int i = index; i < count(); i += 1) {
			int val = int32At(i);
			if (val < 0) {
				return -1;
			}
			if (val > 0) {
				return +1;
			}
		}
		return 0;
	}

	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof Int32Array) {
			Int32Array o = (Int32Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int32At(i + start) + o.int32At(i + otherStart);
				storeInt32(i + start, resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof Int32Array) {
			Int32Array o = (Int32Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int32At(i + start) - o.int32At(i + otherStart);
				storeInt32(i + start, resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(int32At(index));
	}

	public int intAt(int estateIndex) {
		throw new UnsupportedOperationException();
	}

	public void storeInt(int index, int value) {
		storeInt32(index, value);
	}

	public void store(int index, int value) {
		storeInt32(index, value);
	}

	public void storeUInt(int index, int value) {
		storeInt32(index, value);
	}

	public int uIntAt(int index) {
		return int32At(index);
	}
	
	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendUInt32(count());
		for (int i = 0; i < storage.length; i++) {
			int value = storage[i];
			xmtr.sendInt32(value);
		}
	}

}
