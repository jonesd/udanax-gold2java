/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * $Id$
 */
package org.abora.gold.collection.basic;

import java.io.PrintWriter;

import org.abora.gold.spaces.integers.IntegerPos;
import org.abora.gold.x.PrimIntegerSpec;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

/**
 * Concrete fixed size array that holds elements of the 16-bit signed integral type.
 * This maps to the Java <code>short</int> primitive type.
 */

public class Int64Array extends PrimIntArray {
	private final long[] storage;

	//////////////////////////////////////////////
	// Constructors

	protected Int64Array(int count) {
		super();
		storage = new long[count];
	}

	protected Int64Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected Int64Array(long[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an Int64Array filled with zeros */
	public static Int64Array make(int count) {
		return new Int64Array(count);
	}

	/** create an Int64Array filled with the indicated data in 'from' */
	public static Int64Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new Int64Array(size, from, sourceOffset, count, destOffset);
	}

	public static Int64Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static Int64Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static Int64Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static Int64Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an Int64Array filled with the data at 'buffer' */
	public static Int64Array make(long[] buffer) {
		return new Int64Array(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an 64 bit signed integer value */
	public void storeInt64(int index, long value) {
		storage[index] = value;
	}

	/** Get an 64 bit signed actual integer value */
	public long int64At(int index) {
		return storage[index];
	}

	public void storeInteger(int index, int value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeInt64(index, value);
	}

	public int integerAt(int index) {
		//TODO review
		return (int)int64At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		IntegerPos integerValue = (IntegerPos)value;
		//TODO review
		storeInteger(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		//TODO review cast
		return IntegerPos.make((int)int64At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
		//return PrimSpec.int64();
	}

	public int bitCount() {
		return -64;
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
	public void copyToBuffer(long[] buffer, int count, int start) {
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
		if (other instanceof Int64Array) {
			Int64Array o = (Int64Array) other;
			for (int i = 0; i < count; i += 1) {
				long cmp1 = int64At(i + start);
				long cmp2 = o.int64At(i + otherStart);
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
			long val = int64At(i);
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
		if (other instanceof Int64Array) {
			Int64Array o = (Int64Array) other;
			for (int i = 0; i < count; i += 1) {
				long resultant = int64At(i + start) + o.int64At(i + otherStart);
				storeInt64(i + start, resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof Int64Array) {
			Int64Array o = (Int64Array) other;
			for (int i = 0; i < count; i += 1) {
				long resultant = int64At(i + start) - o.int64At(i + otherStart);
				storeInt64(i + start, resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(int64At(index));
	}
}
