/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.collection.basic;

import org.abora.gold.java.missing.BooleanVar;
import org.abora.gold.java.missing.smalltalk.Stream;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Xmtr;
import org.abora.gold.xpp.basic.Heaper;

public class PtrArray extends PrimArray {

	//	protected PtrArray (Int32 count, TCSJ);

	protected PtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected PtrArray(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create a PtrArray filled with NULLs */
	public static PtrArray nulls(int count) {
		throw new UnsupportedOperationException();
	}

	/** create a PtrArray filled with the indicated data in 'from' */
	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static PtrArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static PtrArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create a PtrArray filled with data from 'buffer' */
	public static PtrArray make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create a zero size PtrArray */
	public static PtrArray empty() {
		throw new UnsupportedOperationException();
	}

	public void store(int index, Heaper pointer) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Retrieve a single element from the array. Does array bounds checking.
	 * BLAST if NULL
	 */
	public Heaper get(int index) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Retrieve a single element from the array. Does array bounds checking.
	 * Non-pointer arrays box up the contents in a PrimValue object.
	 */
	public Heaper fetch(int index) {
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

	public BooleanVar contentsEQ(PtrArray other) {
		throw new UnsupportedOperationException();
	}

	public BooleanVar contentsEqual(PrimArray other) {
		throw new UnsupportedOperationException();
	}

	public int contentsHash() {
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

	public int indexOfEQ(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexOfEQ(Heaper value, int start) {
		return indexOfEQ(value, start, 1);
	}

	public int indexOfEQ(Heaper value) {
		return indexOfEQ(value, 0);
	}

	public int indexOfEQOrNull(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexOfEQOrNull(Heaper value, int start) {
		return indexOfEQOrNull(value, start, 1);
	}

	public int indexOfEQOrNull(Heaper value) {
		return indexOfEQOrNull(value, 0);
	}

	public int indexPastEQ(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexPastEQ(Heaper value, int start) {
		return indexPastEQ(value, start, 1);
	}

	public int indexPastEQ(Heaper value) {
		return indexPastEQ(value, 0);
	}

	public BooleanVar elementsEQ(int here, PrimArray other, int there, int count) {
		throw new UnsupportedOperationException();
	}

	public BooleanVar elementsEQ(int here, PrimArray other, int there) {
		return elementsEQ(here, other, there, -1);
	}

	public BooleanVar elementsEQ(int here, PrimArray other) {
		return elementsEQ(here, other, 0);
	}

	public BooleanVar elementsEqual(int here, PrimArray other, int there, int count) {
		throw new UnsupportedOperationException();
	}

	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
	}

	private void receivePtrArray(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	private void sendPtrArray(Xmtr xmtr) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, Stream oo) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	/** for bulk methods that need checking and for migration */
	public void unsafeStore(int index, Heaper ptr) {
		throw new UnsupportedOperationException();
	}

	/** for bulk methods that need checking and for migration */
	public Heaper "*" unsafeFetch(int index) {
		throw new UnsupportedOperationException();
	}

	public void migrate(int[] destination, BooleanVar destinationIsOld) {
		throw new UnsupportedOperationException();
	}

	private void nullEntry() {
		throw new UnsupportedOperationException();
	}

}
