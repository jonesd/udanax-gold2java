/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.collection.basic;

import org.abora.gold.java.missing.smalltalk.Stream;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

public class IEEE64Array extends PrimFloatArray {

	protected IEEE64Array(int count, int datumSize) {
		super(count, datumSize);
	}

	//	IEEE64Array (Int32 count, TCSJ);

	protected IEEE64Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected IEEE64Array(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create an IEEE64 array filled with zeros */
	public static IEEE64Array make(int count) {
		throw new UnsupportedOperationException();
	}

	/** create an IEEE64Array filled with the indicated data in 'from' */
	public static IEEE64Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static IEEE64Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IEEE64Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IEEE64Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create an IEEE64Array filled with the data at 'buffer' */
	public static IEEE64Array make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** Store an actual floating point value */
	public void storeIEEE64(int index, double value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual floating point number */
	public double iEEE64At(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeFloat(int index, double value) {
		throw new UnsupportedOperationException();
	}

	public double floatAt(int index) {
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

	/** Return the maximum word size that can be stored in this array */
	public int bitCount() {
		throw new UnsupportedOperationException();
	}

	public void storeAll(Heaper value, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void zeroElements(int from, int count) {
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
