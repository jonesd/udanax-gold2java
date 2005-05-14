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
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Xmtr;
import org.abora.gold.xpp.basic.Heaper;

public class UInt8Array extends PrimIntArray {

	//	protected UInt8Array (Int32 count, TCSJ);

	protected UInt8Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(-1, -1);
		throw new UnsupportedOperationException();
	}

	protected UInt8Array(int count, int[] buffer) {
		super(-1, -1);
		throw new UnsupportedOperationException();
	}

	/** create a UInt8Array filled with zeros */
	public static UInt8Array make(int count) {
		throw new UnsupportedOperationException();
	}

	/** create a UInt8Array filled with the indicated data in 'from' */
	public static UInt8Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
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

	/** create a UInt8Array filled with the data at 'buffer' */
	public static UInt8Array make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/**
	 * create a UInt8Array of size strlen(string) filled with the contents of
	 * the string (keep the '\0' ?)
	 */
	public static UInt8Array string(String string) {
		throw new UnsupportedOperationException();
	}

	/** Store a 32 bit unsigned integer value */
	public void storeUInt(int index, int value) {
		throw new UnsupportedOperationException();
	}
	
	//TODO duplicated storeUInt
	public void put(int index, int value) {
		throw new UnsupportedOperationException();
	}

	/** Get a 32 bit unsigned actual integer value */
	public int uIntAt(int index) {
		throw new UnsupportedOperationException();
	}

	//TODO duplicated uIntAt
	public int at(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeInteger(int index, /* IntegerVar */ int value) {
		throw new UnsupportedOperationException();
	}

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

	public void storeMany(int to, PrimArray other, int count, int from) {
		throw new UnsupportedOperationException();
	}
	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void zeroElements(int from, int count) {
		throw new UnsupportedOperationException();
	}

	public void printOn(Stream oo) {
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
		throw new UnsupportedOperationException();
	}
	public void noMoreGuts() {
		throw new UnsupportedOperationException();
	}

	private void receiveUInt8Array(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	private void sendUInt8Array(Xmtr xmtr) {
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
