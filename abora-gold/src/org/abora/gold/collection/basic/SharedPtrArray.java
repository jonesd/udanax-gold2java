/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.collection.basic;

import org.abora.gold.x.PrimSpec;

public class SharedPtrArray extends PtrArray {

	private int myShareCount;

	//	protected SharedPtrArray (Int32 count, TCSJ);

	protected SharedPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected SharedPtrArray(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create a PtrArray filled with NULLs */
	public static SharedPtrArray make(int count) {
		throw new UnsupportedOperationException();
	}

	/** create a SharedPtrArray filled with the indicated data in 'from' */
	public static SharedPtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static SharedPtrArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static SharedPtrArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static SharedPtrArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create a PtrArray filled with the data from 'buffer' */
	public static SharedPtrArray make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
	}

	public int shareCount() {
		throw new UnsupportedOperationException();
	}

	public void shareLess() {
		throw new UnsupportedOperationException();
	}

	public void shareMore() {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}
}
