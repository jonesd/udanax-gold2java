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
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.x.PrimIntegerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class UInt32Array extends PrimIntArray {
	private final int[] storage;

	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(UInt32Array.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	//////////////////////////////////////////////
	// Constructors

	protected UInt32Array(int count) {
		super();
		this.storage = new int[count];
	}
	
	public UInt32Array(Rcvr rcvr) {
		super(rcvr);
		int count = rcvr.receiveUInt32();
		storage = new int[count];
		for (int i = 0; i < storage.length; i++) {
			storeInteger(i, rcvr.receiveUInt32());
		}
	}

	
	protected UInt32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected UInt32Array(long[] buffer) {
		this(buffer.length);
		for (int i = 0; i < buffer.length; i++) {
			long uInt32 = buffer[i];
			storage[i] = toSignedByte(uInt32);
		}
	}

	////////////////////////////////////////////////////////////////////////////
	// Unsigned Util
	
	private long toUnsignedInt(int int32) {
		return (long)(int32 & 0xffffffffL);
	}
	
	private int toSignedByte(long uInt32) {
		return (int)(uInt32 & 0xffffffffL);
	}


	//////////////////////////////////////////////
	// Static Factory Methods
	
	/** create a UInt32Array filled with zeros */
	public static UInt32Array make(int count) {
		return new UInt32Array(count);
	}

	/** create a UInt32Array filled with the indicated data in 'from' */
	public static UInt32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new UInt32Array(size, from, sourceOffset, count, destOffset);
	}

	public static UInt32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static UInt32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static UInt32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static UInt32Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create a UInt32Array filled with the data at 'buffer' */
	public static UInt32Array make(long[] buffer) {
		return new UInt32Array(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}


	//////////////////////////////////////////////
	// Accessing

	/** Store a 32 bit unsigned integer value */
	public void storeUInt32(int index, long value) {
		storage[index] = toSignedByte(value);
	}

	/** Get a 32 bit unsigned actual integer value */
	public long uInt32At(int index) {
		return toUnsignedInt(storage[index]);
	}

	public void storeInteger(int index, int value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange: "+value+" spec: "+spec());
		}
		//TODO review
		storeUInt32(index, value);
	}

	public int integerAt(int index) {
		//TODO review
		return (int)uInt32At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		IntegerPos integerValue = (IntegerPos)value;
		storeInteger(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		//TODO review
		return IntegerPos.make((int)uInt32At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		return PrimSpec.uInt32();
	}

	public int bitCount() {
		return 32;
	}


	//////////////////////////////////////////////
	// Bulk Storing
	
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
		for (int i = 0; i < n; i++) {
			long uInt32 = uInt32At(start + i);
			buffer[i] = uInt32;
		}
	}


	//////////////////////////////////////////////
	// Comparison and Hashing
	
	protected int compareData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt32Array) {
			UInt32Array o = (UInt32Array) other;
			for (int i = 0; i < count; i += 1) {
				long cmp = uInt32At(i + start) - o.uInt32At(i + otherStart);
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
			long value = uInt32At(i);
			if (value > 0) {
				return +1;
			}
		}
		return 0;
	}


	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt32Array) {
			UInt32Array o = (UInt32Array) other;
			for (int i = 0; i < count; i += 1) {
				long resultant = uInt32At(i + start) + o.uInt32At(i + otherStart);
				storeUInt32(i + start, resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt32Array) {
			UInt32Array o = (UInt32Array) other;
			for (int i = 0; i < count; i += 1) {
				long resultant = uInt32At(i + start) - o.uInt32At(i + otherStart);
				storeUInt32(i + start, resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}


	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(uInt32At(index));
	}

	public void storeUInt(int index, int value) {
		//TODO do we need this method?
		storeUInt32(index, value);
	}

	public int uIntAt(int index) {
		//TODO this is probably wrong!
		return integerAt(index);
	}

	public void store(int index, int value) {
		//TODO do we need this method?
		storeUInt32(index, value);
	}
	
	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendUInt32(count());
		for (int i = 0; i < storage.length; i++) {
			int value = storage[i];
			xmtr.sendUInt32(value);
		}
	}

}
