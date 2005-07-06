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

public class Int16Array extends PrimIntArray {
	private final short[] storage;

	//////////////////////////////////////////////
	// Constructors

	protected Int16Array(int count) {
		super();
		storage = new short[count];
	}

	protected Int16Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected Int16Array(short[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an Int16Array filled with zeros */
	public static Int16Array make(int count) {
		return new Int16Array(count);
	}

	/** create an Int16Array filled with the indicated data in 'from' */
	public static Int16Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new Int16Array(size, from, sourceOffset, count, destOffset);
	}

	public static Int16Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static Int16Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static Int16Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static Int16Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an Int16Array filled with the data at 'buffer' */
	public static Int16Array make(short[] buffer) {
		return new Int16Array(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an 16 bit signed integer value */
	public void storeInt16(int index, short value) {
		storage[index] = (short) value;
	}

	/** Get an 16 bit signed actual integer value */
	public short int16At(int index) {
		return storage[index];
	}

	public void storeInteger(int index, int value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeInt16(index, (short)value);
	}

	public int integerAt(int index) {
		return int16At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		IntegerPos integerValue = (IntegerPos)value;
		storeInteger(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		return IntegerPos.make(int16At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
		//return PrimSpec.int16();
	}

	public int bitCount() {
		return -16;
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
	public void copyToBuffer(short[] buffer, int count, int start) {
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
		if (other instanceof Int16Array) {
			Int16Array o = (Int16Array) other;
			for (int i = 0; i < count; i += 1) {
				int cmp = int16At(i + start) - o.int16At(i + otherStart);
				if (cmp != 0) {
					return cmp < 0 ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int index) {
		for (int i = index; i < count(); i += 1) {
			short val = int16At(i);
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
		if (other instanceof Int16Array) {
			Int16Array o = (Int16Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int16At(i + start) + o.int16At(i + otherStart);
				storeInt16(i + start, (short) resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof Int16Array) {
			Int16Array o = (Int16Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int16At(i + start) - o.int16At(i + otherStart);
				storeInt16(i + start, (short) resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(int16At(index));
	}
}
