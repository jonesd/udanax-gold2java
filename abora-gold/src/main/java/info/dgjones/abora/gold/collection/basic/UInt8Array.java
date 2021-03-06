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
import java.util.Arrays;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimIntegerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class UInt8Array extends PrimIntArray {
	private final byte[] storage;

	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(UInt8Array.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	//////////////////////////////////////////////
	// Constructors

	//TODO want this constructor to be protected
	public UInt8Array(int count) {
		super();
		storage = new byte[count];
	}
	
	public UInt8Array(Rcvr rcvr) {
		super(rcvr);
		int count = rcvr.receiveUInt32();
		storage = new byte[count];
		for (int i = 0; i < storage.length; i++) {
			storeInteger(i, rcvr.receiveUInt8());
		}
	}


	protected UInt8Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected UInt8Array(short[] buffer) {
		this(buffer.length);
		for (int i = 0; i < buffer.length; i++) {
			short s = buffer[i];
			storage[i] = toSignedByte(s);
		}
	}
	
	protected UInt8Array(byte[] capturedBuffer) {
		super();
		storage = capturedBuffer;
	}
	

	////////////////////////////////////////////////////////////////////////////
	// Unsigned Util
	
	private short toUnsignedByte(byte b) {
		return (short)(b & 0xff);
	}
	
	private byte toSignedByte(short s) {
		return (byte)(s & 0xff);
	}
	
	
	//////////////////////////////////////////////
	// Static Factory Methods

	/** create a UInt8Array filled with zeros */
	public static UInt8Array make(int count) {
		return new UInt8Array(count);
	}

	/** create a UInt8Array filled with the indicated data in 'from' */
	public static UInt8Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new UInt8Array(size, from, sourceOffset, count, destOffset);
	}

	public static UInt8Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static UInt8Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static UInt8Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static UInt8Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create a UInt8Array filled with the data at 'buffer' */
	public static UInt8Array make(short[] buffer) {
		return new UInt8Array(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}


	//////////////////////////////////////////////
	// Accessing

	public void storeUInt8(int index, short value) {
		storage[index] = toSignedByte(value);
	}

	public short uInt8At(int index) {
		return toUnsignedByte(storage[index]);
	}

	public void storeInteger(int index, int value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		//TODO review
		storeUInt8(index, (short)value);
	}

	public int integerAt(int index) {
		return uInt8At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		PrimIntValue integerValue = (PrimIntValue)value;
		storeInteger(index, integerValue.asInt32());
	}

	public Heaper fetchValue(int index) {
		return PrimIntValue.make(uInt8At(index));
	}

	public PrimSpec spec() {
		return PrimSpec.uInt8();
	}

	public int bitCount() {
		return 8;
	}

	public int count() {
		return storage.length;
	}


	//////////////////////////////////////////////
	// Bulk Storage

//	public void storeMany(int to, PrimArray other, int count, int from) {
//		throw new UnsupportedOperationException();
//	}
	
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
		for (int i = 0; i < n; i++) {
			short s = uInt8At(start + i);
			buffer[i] = s;
		}
	}

//	public void zeroElements(int from, int count) {
//		throw new UnsupportedOperationException();
//	}

//	protected void copyElements(int to, PrimArray source, int from, int count) {
//		throw new UnsupportedOperationException();
//	}


	//////////////////////////////////////////////
	// Comparing and Hashing

	protected int compareData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt8Array) {
			UInt8Array o = (UInt8Array) other;
			for (int i = 0; i < count; i += 1) {
				int cmp = uInt8At(i + start) - o.uInt8At(i + otherStart);
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
			short value = uInt8At(i);
			if (value > 0) {
				return +1;
			}
		}
		return 0;
	}


	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt8Array) {
			UInt8Array o = (UInt8Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = uInt8At(i + start) + o.uInt8At(i + otherStart);
				storeUInt8(i + start, (short) resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof UInt8Array) {
			UInt8Array o = (UInt8Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = uInt8At(i + start) - o.uInt8At(i + otherStart);
				storeUInt8(i + start, (short) resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}


	//////////////////////////////////////////////
	// Printing

//	public void printOn(PrintWriter oo) {
//		throw new UnsupportedOperationException();
//	}

	public void printOn(PrintWriter oo) {
		if (count() == 0) {
			oo.print("[empty]");
		} else {
			for (int i = 0; i < count(); i += 1) {
				printElementOn(i, oo);
			}
		}
	}
	
	protected void printElementOn(int index, PrintWriter oo) {
		oo.print((char)uInt8At(index));
	}

	//////////////////////////////////////////////
	// Conversions
	
	public String asString() {
		throw new UnsupportedOperationException();
	}

	public static UInt8Array string(String string) {
		//TODO wrong!
		UInt8Array array = UInt8Array.make(string.length());
		for (int i = 0; i < string.length(); i++) {
			int value = string.charAt(i);
			array.storeUInt8(i, (short)value);
			//array.storeInteger(i, value);
		}
		return array;
	}

	public int at(int index) {
		return uInt8At(index);
	}

	public static void bombReleaseGuts(UInt8Array buffer) {
		throw new UnsupportedOperationException();
	}
	
	/**	
	 * A pointer to the actual string.  While one of these are outstanding, one
	 * may not allocate any PrimArrays, because doing so may cause compaction,
	 * which would relocate the data.  In order to keep track of whether there
	 * are outstanding hard pointers, my clients must call noMoreGuts() when
	 * they will no longer be using the pointer.
	 */
	public PrimArray /*TODO should be String?*/ gutsOf() {
		//TODO making this up...
		return this;
	}
	public void noMoreGuts() {
		//TODO making this up...
	}

	public int uIntAt(int myIndex) {
		return integerAt(myIndex);
	}

	public void storeUInt(int index, int value) {
		storeInteger(index, value);
	}

	public void put(int index, int value) {
		storeInteger(index, value);
	}

	public static UInt8Array basicNew() {
		throw new UnsupportedOperationException();
	}
	
	public int int32At(int index) {
        int value = 0;
        for (int i = 0; i < 4; i++) {
            int shift = (4 - 1 - i) * 8;
            value += (storage[i + index] & 0x000000FF) << shift;
        }
        return value;	
       }

	public void storeInt32(int index, int value) {
		 for (int i = 0; i < 4; i++) {
	            int offset = (3 - i) * 8;
	            storage[i + index] = (byte) ((value >>> offset) & 0xFF);
	        }
		}

	public static UInt8Array makeShared(byte[] capturedBuffer) {
		return new UInt8Array(capturedBuffer);
	}

	public byte[] gutsOfByteArray() {
		return storage;
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendUInt32(count());
		for (int i = 0; i < storage.length; i++) {
			int value = storage[i];
			xmtr.sendUInt8(value);
		}
	}
	
	public void copyBytes(int source, int destination, int count) {
		System.arraycopy(storage, source, storage, destination, count);
	}
	
	public String toString() {
		//TODO we mess around with contents of bytearray to generate unsigned...
		int arrayEnd = storage.length;
		for (int i = 0; i < storage.length; i++) {
			int value = storage[i];
			if (value == 0) {
				arrayEnd = i;
				break;
			}
		}
		if (arrayEnd > 0) {
			return new String(storage, 0, arrayEnd);
		} else {
			return "[empty]";
		}
	}

	public int compareTo(String string) {
		return toString().compareTo(string);
	}

}
