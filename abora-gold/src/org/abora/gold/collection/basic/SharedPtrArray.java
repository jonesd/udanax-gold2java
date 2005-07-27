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

import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

public class SharedPtrArray extends PtrArray {
	private int myShareCount = 0;

	//////////////////////////////////////////////
	// Constructors

	protected SharedPtrArray(int count) {
		super(count);
	}

	protected SharedPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(size, from, sourceOffset, count, destOffset);
	}

	protected SharedPtrArray(Heaper[] buffer) {
		super(buffer);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create a PtrArray filled with NULLs */
	public static PtrArray make(int count) {
		return new SharedPtrArray(count);
	}

	/** create a SharedPtrArray filled with the indicated data in 'from' */
	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new SharedPtrArray(size, from, sourceOffset, count, destOffset);
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

	public static PtrArray make(PrimArray from) {
		return make(from.count(), from, 0);
	}

	/** create a PtrArray filled with the data from 'buffer' */
	public static PtrArray make(Heaper[] buffer) {
		return new SharedPtrArray(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PtrArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	public PrimSpec spec() {
		return PrimSpec.sharedPointer();
	}

	public int shareCount() {
		return myShareCount;
	}

	public void shareLess() {
		myShareCount -= 1;
		//TODO should we throw an exception or just ground myShareCount to 0?
		if (myShareCount < 0) {
			throw new AboraRuntimeException("Share fallen below 0: "+this);
		}
	}

	public void shareMore() {
		myShareCount += 1;
	}
}
