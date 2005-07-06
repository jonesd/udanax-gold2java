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
import java.io.UnsupportedEncodingException;

import org.abora.gold.spaces.integers.IntegerPos;
import org.abora.gold.x.PrimIntegerSpec;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

/**
 * Concrete fixed size array that holds elements of the 8-bit signed integral type.
 * This maps to the Java <code>byte</int> primitive type.
 */
public class Int8Array extends PrimIntArray {
	private final byte[] storage;

	//////////////////////////////////////////////
	// Constructors

	protected Int8Array(int count) {
		super();
		storage = new byte[count];
	}

	protected Int8Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected Int8Array(byte[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an Int8Array filled with zeros */
	public static Int8Array make(int count) {
		return new Int8Array(count);
	}

	/** create an Int32Array filled with the indicated data in 'from' */
	public static Int8Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new Int8Array(size, from, sourceOffset, count, destOffset);
	}

	public static Int8Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static Int8Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static Int8Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static Int8Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an Int8Array filled with the data at 'buffer' */
	public static Int8Array make(byte[] buffer) {
		return new Int8Array(buffer);
	}

	/**
	 * Return a new array filled with the specified ASCII string.
	 * String characters are assumed to be ASCII values.
	 * 
	 * @param string string to fill array with.
	 * @return a new array filled with the specified string.
	 */
	public static Int8Array asciiString(String string) {
		//TODO should we be catching encoding exception?
		try {
			return new Int8Array(string.getBytes("US-ASCII"));
		} catch (UnsupportedEncodingException e) {
			throw new IllegalStateException(e.toString());
		}
	}

	/**
	 * Return a new array filled with the specified string in UTF-8 format.
	 * 
	 * @param string string to fill array with.
	 * @return a new array filled with the specified string.
	 */
	public static Int8Array utf8String(String string) {
		//TODO should we be catching encoding exception?
		try {
			return new Int8Array(string.getBytes("UTF-8"));
		} catch (UnsupportedEncodingException e) {
			throw new IllegalStateException(e.toString());
		}
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an 8 bit signed integer value */
	public void storeInt8(int index, byte value) {
		storage[index] = value;
	}

	/** Get an 8 bit signed actual integer value */
	public byte int8At(int index) {
		return storage[index];
	}

	public void storeInteger(int index, int value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeInt8(index, (byte)value);
	}

	public int integerAt(int index) {
		return int8At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		IntegerPos integerValue = (IntegerPos)value;
		storeInteger(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		return IntegerPos.make(int8At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
		//return PrimSpec.int8();
	}

	public int bitCount() {
		return -8;
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
	public void copyToBuffer(byte[] buffer, int count, int start) {
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
		if (other instanceof Int8Array) {
			Int8Array o = (Int8Array) other;
			for (int i = 0; i < count; i += 1) {
				int cmp = int8At(i + start) - o.int8At(i + otherStart);
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
			byte val = int8At(i);
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
		if (other instanceof Int8Array) {
			Int8Array o = (Int8Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int8At(i + start) + o.int8At(i + otherStart);
				storeInt8(i + start, (byte) resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof Int8Array) {
			Int8Array o = (Int8Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int8At(i + start) - o.int8At(i + otherStart);
				storeInt8(i + start, (byte) resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(int8At(index));
	}
}
