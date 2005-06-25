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

public class PrimIntArray extends PrimIntegerArray {

	protected PrimIntArray(int count, int datumSize) {
		super(count, datumSize);
	}

	/**	
	 * Make an array initialized to zeros. The values are signed if bitCount is
	 * negative
	 */
	public static PrimIntArray zeros(/* IntegerVar */ int bitCount, /* IntegerVar */ int count) {
		throw new UnsupportedOperationException();
	}

	/** Store an integer value */
	public void storeInteger(int index, /* IntegerVar */ int value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual integer value */
	public /* IntegerVar */ int integerAt(int index) {
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

	public int bitCount() {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
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
