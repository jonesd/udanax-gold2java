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

import info.dgjones.abora.gold.xcvr.Rcvr;


public abstract class PrimIntArray extends PrimIntegerArray {

	//////////////////////////////////////////////
	// Constructors

	protected PrimIntArray() {
		super();
	}
	
	public PrimIntArray(Rcvr rcvr) {
		super(rcvr);
	}


	//////////////////////////////////////////////
	// Static Factory Methods

	/**	
	 * Make an array initialized to zeros. The values are signed if bitCount is
	 * negative
	 */
	public static PrimIntArray zeros(int numBits, int count) {
		if (numBits == 8) {
			return UInt8Array.make(count);
		}
		if (numBits == 32) {
			//TODO really requsting for an unsigned array here...
			//return UInt32Array.make(count);
			return Int32Array.make(count);
		}
		if (numBits == -32) {
			return Int32Array.make(count);
		}
		throw new IllegalArgumentException("UnimplementedPrecision");
		//		RPTR(PrimIntArray) PrimIntArray::zeros (IntegerVar numBits,
		//							IntegerVar count)
		//		{
		//			/* Make an array initialized to zero values. The values are
		//			   signed if numBits is negative. */
		//
		//			if (numBits == 8) {
		//			return UInt8Array::make (count.asInt32());
		//			}
		//			if (numBits == 32) {
		//			return UInt32Array::make (count.asInt32());
		//			}
		//			if (numBits == -32) {
		//			return Int32Array::make (count.asInt32());
		//			}
		//			BLAST(UnimplementedPrecision);
		//			/* compiler fodder */
		//			return NULL;
		//		}
	}

	//////////////////////////////////////////////
	// Accessing

	/**
	 * Return the maximum bits/entry that can be stored in this array
	 */
	public abstract int bitCount();

	public void copyToBuffer(PrimArray array, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}
}
