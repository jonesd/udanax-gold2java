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

/**
 * A common superclass for primitive arrays of basic data
 * types (i.e. bits of some kind)
 */
public class PrimDataArray extends PrimArray {

	protected PrimDataArray(int count, int datumSize) {
		super(count, datumSize);
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

	public boolean contentsEqual(PrimArray other) {
		throw new UnsupportedOperationException();
	}

	public void addElements(int to, PrimDataArray other, int count, int from) {
		throw new UnsupportedOperationException();
	}

	public void addElements(int to, PrimDataArray other, int count) {
		addElements(to, other, count, 0);
	}

	public void addElements(int to, PrimDataArray other) {
		addElements(to, other, -1);
	}

	public void subtractElements(int to, PrimDataArray other, int count, int from) {
		throw new UnsupportedOperationException();
	}

	public void subtractElements(int to, PrimDataArray other, int count) {
		subtractElements(to, other, count, 0);
	}

	public void subtractElements(int to, PrimDataArray other) {
		subtractElements(to, other, -1);
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

	/**
	 * Return -1, 0, or +1 according to whether the elements in the specified
	 * span of this array are lexically less than, equal to, or greater than the
	 * specified span of the other. The other array must be of a compatible
	 * type. If the count is negative or goes beyond the end of either array,
	 * then the shorter array is considered to be extended with zeros. 	NOTE:
	 * Because of zero extension, this is not the same as elementsEqual; it is
	 * possible that a->compare (b) == 0 even though ! a->contentsEqual (b)
	 */
	public int compare(PrimDataArray other, int count, int here, int there) {
		throw new UnsupportedOperationException();
	}

	public int compare(PrimDataArray other, int count, int here) {
		return compare(other, count, here, 0);
	}

	public int compare(PrimDataArray other, int count) {
		return compare(other, count, 0);
	}

	public int compare(PrimDataArray other) {
		return compare(other, -1);
	}

	public boolean elementsEqual(int here, PrimArray other, int there, int count) {
		throw new UnsupportedOperationException();
	}

	/** A hash of the range of values out of the array */
	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
	}

	/**
	 * over given range, returns - if this < other; 0 if this == other; + if
	 * this > other.
	 */
	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	/**
	 * return the sign of the next non-zero element after start, or 0 if no such
	 * element.  Note that for the unsigned arrays, this will only return 0 or
	 * 1.
	 */
	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Add the respective elements of other to this over the given index range.
	 */
	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Subtract the respective elements of other from this over the given index
	 * range.
	 */
	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, Stream oo) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

}
