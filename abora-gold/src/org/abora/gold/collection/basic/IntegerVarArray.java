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
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

public class IntegerVarArray extends PrimIntegerArray {
	private final int[] storage;


	//////////////////////////////////////////////
	// Constructors

	protected IntegerVarArray(int count) {
		super();
		storage = new int[count];
		zeroElements();
	}

	protected IntegerVarArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		//ALREADY zeroElements (0, destOffset);
		copyElements(destOffset, from, sourceOffset, n);
		//ALREADY zeroElements (destOffset + count, size - destOffset - count);
	}

	protected IntegerVarArray(int[] source) {
		this(source.length);
		//TODO inefficiency of zeroing array here
		System.arraycopy(source, 0, storage, 0, source.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an IntegerVarArray filled with zeros */
	public static IntegerVarArray make(int count) {
		return new IntegerVarArray(count);
	}

	/** create an IntegerVarArray filled with the indicated data in 'from' */
	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new IntegerVarArray(size, from, sourceOffset, count, destOffset);
	}

	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IntegerVarArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static IntegerVarArray make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an IntegerVarArray filled with the data at 'buffer' */
	public static IntegerVarArray make(int[] buffer) {
		return new IntegerVarArray(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an actual integer value */
	public void storeIntegerVar(int index, int value) {
		storage[index] = value;
	}

	/** Get an actual integer value */
	public int integerVarAt(int index) {
		return storage[index];
	}

	public void storeInteger(int index, int value) {
		storeIntegerVar(index, value);
	}

	public int integerAt(int index) {
		return integerVarAt(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		IntegerPos integerValue = (IntegerPos)value;
		storeIntegerVar(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		return IntegerPos.make(integerVarAt(index));
	}

	public PrimSpec spec() {
		return PrimSpec.integerVar();
	}

	public int count() {
		return storage.length;
	}

	//////////////////////////////////////////////
	// Bulk Storage

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

	protected void copyElements(int to, PrimArray source, int from, int count) {
		int n = count;
		if (n == -1) {
			n = source.count() - from;
		}
		PrimIntegerArray s = (PrimIntegerArray) source;
		for (int i = 0; i < n; i += 1) {
			storeIntegerVar(to + i, s.integerAt(from + i));
		}
	}

	//////////////////////////////////////////////
	// Comparing and Hashing

	protected int compareData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IntegerVarArray) {
			IntegerVarArray o = (IntegerVarArray) other;
			for (int i = 0; i < count; i += 1) {
				int a = integerVarAt(start + i);
				int b = o.integerVarAt(otherStart + i);
				int comparison = compare(a, b);
				if (comparison != 0) {
					return comparison;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int index) {
		for (int i = index; i < count(); i += 1) {
			int val = integerVarAt(i);
			int comparison = compare(val, 0);
			if (comparison != 0) {
				return comparison;
			}
		}
		return 0;
	}
	
	//////////////////////////////////////////////
	// Arthmetic Operations

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IntegerVarArray) {
			IntegerVarArray o = (IntegerVarArray) other;
			for (int i = 0; i < count; i += 1) {
				storeIntegerVar(i + start, integerVarAt(i + start) + o.integerVarAt(i + otherStart));
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IntegerVarArray) {
			IntegerVarArray o = (IntegerVarArray) other;
			for (int i = 0; i < count; i += 1) {
				storeIntegerVar(i + start, integerVarAt(i + start) - o.integerVarAt(i + otherStart));
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(integerVarAt(index));
	}

	public static IntegerVarArray zeros(int n) {
		return IntegerVarArray.make(n);
	}
}
