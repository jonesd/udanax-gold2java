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


public abstract class PrimIntArray extends PrimIntegerArray {

	//////////////////////////////////////////////
	// Constructors

	protected PrimIntArray() {
		super();
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
			return UInt32Array.make(count);
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
