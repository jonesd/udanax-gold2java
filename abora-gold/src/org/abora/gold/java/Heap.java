/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java;

import org.abora.gold.collection.basic.PrimArray;

public class Heap {
	//@todo should be called PrimArray Heap - just couldnt create a class with 
	// that name in Eclipse - possibly due to a bug
	private Heap myNextHeap;

	/** start of contiguous space at end */
	private int[] myEndSpace;
	/** table of live PrimArrays on this heap */
	PrimArray  myPrimArrayTable;
	/** Last word in heap */
	int[] myLastWord;
	/** PrimArray table entry after first hole in heap or NULL */
	PrimArray myFirstAfterHole;
	/* the actual heap storage */
	int[] myHeapStore;

	/** list of all PrimArrayHeaps in system */
	static Heap FirstHeap;

	/**
	 * Constructor for Heap.
	 */
	public Heap(int[] size) {
		super();
	}

	/** allocate from particular heap in sizeof(Int32) units and register array */
	public int[] allocate(int nWords, PrimArray pa) {
		throw new UnsupportedOperationException();
	}

	/** compacts array table after GC */
	public void removeDestructedArrays() {
		throw new UnsupportedOperationException();
	}

	private void compact() {
		throw new UnsupportedOperationException();
	}

	/** compacts all array tables after GC */
	public static void cleanup() {
		throw new UnsupportedOperationException();
	}

	/** from any heap */
	public static int[] getStorage(int nWords, PrimArray pa) {
		throw new UnsupportedOperationException();
	}

}
