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

//TODO use of char here is completely wrong!
public class UInt16Array extends PrimIntArray {
	private final char[] storage;

	//////////////////////////////////////////////
	// Constructors

	protected UInt16Array(int count) {
		super();
		storage = new char[count];
	}

	protected UInt16Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected UInt16Array(char[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an UInt16Array filled with zeros */
	public static UInt16Array make(int count) {
		return new UInt16Array(count);
	}

	/** create an Int16Array filled with the indicated data in 'from' */
	public static UInt16Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new UInt16Array(size, from, sourceOffset, count, destOffset);
	}

	public static UInt16Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static UInt16Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static UInt16Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static UInt16Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an UInt16Array filled with the data at 'buffer' */
	public static UInt16Array make(char[] buffer) {
		return new UInt16Array(buffer);
	}

	/**
	 * Return a new array filled with the specified string.
	 * 
	 * @param string string to fill array with.
	 * @return a new array filled with the specified string.
	 */
	public static UInt16Array unicodeString(String string) {
		return make(string.toCharArray());
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an 16 bit unsigned integer value */
	public void storeUInt16(int index, char value) {
		storage[index] = value;
	}

	/** Get an 16 bit signed actual integer value */
	public char uInt16At(int index) {
		return storage[index];
	}

	public void storeInteger(int index, int value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeUInt16(index, (char)value);
	}

	public int integerAt(int index) {
		return uInt16At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		IntegerPos integerValue = (IntegerPos)value;
		storeInteger(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		return IntegerPos.make(uInt16At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
		//return PrimSpec.uInt16();
	}

	public int bitCount() {
		return 16;
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
	public void copyToBuffer(char[] buffer, int count, int start) {
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
		if (other instanceof UInt16Array) {
			UInt16Array o = (UInt16Array) other;
			for (int i = 0; i < count; i += 1) {
				char a = uInt16At(i + start);
				char b = o.uInt16At(i + otherStart);
				if (a < b) {
					return -1;
				} else if (a > b) {
					return 1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int index) {
		for (int i = index; i < count(); i += 1) {
			char val = uInt16At(i);
			if (val > 0) {
				return +1;
			}
		}
		return 0;
	}

	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt16Array) {
			UInt16Array o = (UInt16Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = uInt16At(i + start) + o.uInt16At(i + otherStart);
				storeUInt16(i + start, (char)resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt16Array) {
			UInt16Array o = (UInt16Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = uInt16At(i + start) - o.uInt16At(i + otherStart);
				storeUInt16(i + start, (char)resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		//TODO should this print out in String format instead?
		oo.print((int)uInt16At(index));
	}
	
	//////////////////////////////////////////////
	// Conversions
	
	public String asString() {
		//TODO what to do about the name. don't know whether to stay
		// clear of toString
		
		return String.copyValueOf(storage);
	}

}
