/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.collection.basic;

import org.abora.gold.java.missing.IntegerVar;
import org.abora.gold.java.missing.smalltalk.Stream;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Xmtr;
import org.abora.gold.xpp.basic.Heaper;

public class IntegerVarArray extends PrimIntegerArray {

	//	IntegerVarArray (Int32 count, TCSJ);

	protected IntegerVarArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(-1, -1);
		throw new UnsupportedOperationException();
	}

	protected IntegerVarArray(int count, int[] buffer) {
		super(-1, -1);
		throw new UnsupportedOperationException();
	}

	/** create an IntegerVarArray filled with zeros */
	public static IntegerVarArray zeros(int count) {
		throw new UnsupportedOperationException();
	}

	/** create an IntegerVarArray filled with the indicated data in 'from' */
	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
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

	/** create an IntegerVarArray filled with the data at 'buffer' */
	public static IntegerVarArray make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** Store an actual integer value */
	public void storeIntegerVar(int index, IntegerVar value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual integer value */
	public IntegerVar integerVarAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeInteger(int index, IntegerVar value) {
		throw new UnsupportedOperationException();
	}

	public IntegerVar integerAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeValue(int index, Heaper value) {
		throw new UnsupportedOperationException();
	}

	public Heaper fetchValue(int index) {
		throw new UnsupportedOperationException();
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void zeroElements(int from, int count) {
		throw new UnsupportedOperationException();
	}

	protected void receiveIVArray(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	protected void sendIVArray(Xmtr xmtr) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Only called in construction of fresh array when there is no possibility
	 * of storage leak from skipping IntegerVar assignment. Also, the normal
	 * zeroElements is itself unsafe during construction, because the IntegerVar
	 * code could interpret the random bits in the newly allocated
	 * IntegerVarArray as pointers to bignums to be freed.
	 */
	protected void unsafeZeroElements(int from, int count) {
		throw new UnsupportedOperationException();
	}

	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}

	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, Stream oo) {
		throw new UnsupportedOperationException();
	}

	protected void copyElements(int to, PrimArray source, int from, int count) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

}
