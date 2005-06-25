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

public class PrimFloatArray extends PrimDataArray {

	protected PrimFloatArray(int count, int datumSize) {
		super(count, datumSize);
	}

	/** Make an array initialized to zero values */
	public static PrimFloatArray zeros(int bitCount, int count) {
		throw new UnsupportedOperationException();
	}

	/** Store a floating point value */
	public void storeFloat(int index, double value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual floating point number */
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

	/** Return the maximum bits/entry that can be stored in this array */
	public int bitCount() {
		throw new UnsupportedOperationException();
	}

	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
	}

	public int indexOf(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexPast(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public void storeAll(Heaper value, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(int[] buffer, int size, int count, int start) {
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

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(PrimArray array, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}
}
